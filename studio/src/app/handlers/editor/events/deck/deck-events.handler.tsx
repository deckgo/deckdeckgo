import {ItemReorderEventDetail} from '@ionic/core';

import {Subject, Subscription} from 'rxjs';
import {debounceTime, filter, take} from 'rxjs/operators';

import {AuthUser} from '../../../../models/auth/auth.user';
import {Deck, DeckAttributes, DeckData} from '../../../../models/data/deck';
import {Slide, SlideAttributes, SlideData, SlideTemplate} from '../../../../models/data/slide';

import {Utils} from '../../../../utils/core/utils';
import {Resources} from '../../../../utils/core/resources';

import {SlotUtils} from '../../../../utils/editor/slot.utils';

import {ErrorService} from '../../../../services/core/error/error.service';
import {BusyService} from '../../../../services/editor/busy/busy.service';
import {DeckEditorService} from '../../../../services/editor/deck/deck-editor.service';
import {AuthService} from '../../../../services/auth/auth.service';
import {DeckService} from '../../../../services/data/deck/deck.service';
import {SlideService} from '../../../../services/data/slide/slide.service';

export class DeckEventsHandler {

    private el: HTMLElement;

    private errorService: ErrorService;
    private busyService: BusyService;

    private authService: AuthService;

    private updateSlideSubscription: Subscription;
    private updateSlideSubject: Subject<HTMLElement> = new Subject();

    private deckEditorService: DeckEditorService;

    private updateDeckTitleSubscription: Subscription;
    private updateDeckTitleSubject: Subject<string> = new Subject();

    private deckService: DeckService;
    private slideService: SlideService;

    constructor() {
        this.errorService = ErrorService.getInstance();
        this.busyService = BusyService.getInstance();

        this.authService = AuthService.getInstance();

        this.deckEditorService = DeckEditorService.getInstance();

        this.deckService = DeckService.getInstance();
        this.slideService = SlideService.getInstance();
    }

    init(el: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.el = el;

            this.el.addEventListener('input', this.onInputChange, false);
            this.el.addEventListener('deckDidChange', this.onDeckChange, false);
            this.el.addEventListener('slideDidChange', this.onSlideChange, false);
            this.el.addEventListener('slideDidLoad', this.onSlideDidLoad, false);
            this.el.addEventListener('slidesDidLoad', this.onSlidesDidLoad, false);
            this.el.addEventListener('slideDelete', this.onSlideDelete, false);
            this.el.addEventListener('codeDidChange', this.onCustomEventChange, false);
            this.el.addEventListener('imgDidChange', this.onCustomEventChange, false);
            this.el.addEventListener('linkCreated', this.onCustomEventChange, false);

            this.updateSlideSubscription = this.updateSlideSubject.pipe(debounceTime(500)).subscribe(async (element: HTMLElement) => {
                await this.updateSlide(element);
            });

            this.updateDeckTitleSubscription = this.updateDeckTitleSubject.pipe(debounceTime(500)).subscribe(async (title: string) => {
                await this.updateDeckTitle(title);
            });

            resolve();
        });
    }

    destroy() {
        this.el.removeEventListener('input', this.onInputChange, true);
        this.el.removeEventListener('deckDidChange', this.onDeckChange, true);
        this.el.removeEventListener('slideDidChange', this.onSlideChange, true);
        this.el.removeEventListener('slideDidLoad', this.onSlideDidLoad, true);
        this.el.removeEventListener('slidesDidLoad', this.onSlidesDidLoad, true);
        this.el.removeEventListener('slideDelete', this.onSlideDelete, true);
        this.el.removeEventListener('codeDidChange', this.onCustomEventChange, true);
        this.el.removeEventListener('imgDidChange', this.onCustomEventChange, true);
        this.el.removeEventListener('linkCreated', this.onCustomEventChange, true);

        if (this.updateSlideSubscription) {
            this.updateSlideSubscription.unsubscribe();
        }

        if (this.updateDeckTitleSubscription) {
            this.updateDeckTitleSubscription.unsubscribe();
        }
    }

    private onSlideDidLoad = async ($event: CustomEvent) => {
        if ($event && $event.target && $event.target instanceof HTMLElement) {
            await this.createSlide($event.target);
        }
    };

    private onSlidesDidLoad = async ($event: CustomEvent) => {
        if ($event) {
            await this.initSlideSize();
            await this.slideToLastSlide();
        }
    };

    private onDeckChange = async ($event: CustomEvent) => {
        if (!$event || !$event.detail) {
            return;
        }

        await this.updateDeck($event.detail);
    };

    private onSlideChange = async ($event: CustomEvent) => {
        if (!$event || !$event.detail) {
            return;
        }

        this.updateSlideSubject.next($event.detail);
    };

    private onCustomEventChange = async ($event: CustomEvent) => {
        if (!$event || !$event.detail || !($event.detail instanceof HTMLElement)) {
            return;
        }

        const element: HTMLElement = $event.detail as HTMLElement;

        const parent: HTMLElement = element.parentElement;

        if (!parent || !parent.nodeName || parent.nodeName.toLowerCase().indexOf('deckgo-slide') <= -1) {
            return;
        }

        this.updateSlideSubject.next(parent);
    };

    private onInputChange = async ($event: Event) => {
        if (!$event || !$event.target || !($event.target instanceof HTMLElement)) {
            return;
        }

        const element: HTMLElement = $event.target as HTMLElement;

        let parent: HTMLElement = element.parentElement;

        if (SlotUtils.isNodeReveal(parent)) {
            parent = parent.parentElement;
        }

        if (!parent || !parent.nodeName || parent.nodeName.toLowerCase().indexOf('deckgo-slide') <= -1) {
            return;
        }

        this.updateSlideSubject.next(parent);

        // The first content editable element on the first slide is the title of the presentation
        if (parent && !parent.previousElementSibling && !element.previousElementSibling) {
            this.updateDeckTitleSubject.next(element.textContent);
        }
    };

    private onSlideDelete = async ($event: CustomEvent) => {
        if (!$event || !$event.detail) {
            return;
        }

        await this.deleteSlide($event.detail);
    };

    private createSlide(slide: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            try {
                if (!slide || !slide.nodeName) {
                    resolve();
                    return;
                }

                if (slide.getAttribute('slide_id')) {
                    // !isNew
                    this.busyService.slideEditable(slide);

                    resolve();
                    return;
                }

                this.busyService.deckBusy(true);

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (!deck) {
                        deck = await this.createDeck();
                    }

                    const persistedSlide: Slide = await this.postSlide(deck, slide);

                    // TODO: Ultimately, when using reactive data, move this to a Cloud Function
                    await this.updateDeckSlideList(deck, persistedSlide);

                    this.busyService.deckBusy(false);

                    resolve();
                });
            } catch (err) {
                this.errorService.error(err);
                this.busyService.deckBusy(false);
                resolve();
            }
        });
    }

    private postSlide(deck: Deck, slide: HTMLElement): Promise<Slide> {
        return new Promise<Slide>(async (resolve) => {
            const slideData: SlideData = {
                template: this.getSlideTemplate(slide)
            };

            const content: string = await this.cleanSlideContent(slide);
            if (content && content.length > 0) {
                slideData.content = content
            }

            const attributes: SlideAttributes = await this.getSlideAttributes(slide);

            if (attributes && Object.keys(attributes).length > 0) {
                slideData.attributes = attributes;
            }

            const persistedSlide: Slide = await this.slideService.create(deck.id, slideData);

            if (persistedSlide && persistedSlide.id) {
                slide.setAttribute('slide_id', persistedSlide.id);

                // Add a key to uniquely identify the slide. Mandatory notably for the reorder function
                // See https://stenciljs.com/docs/templating-jsx?#loops
                slide.setAttribute('key', persistedSlide.id);

                this.busyService.slideEditable(slide);
            }

            resolve(persistedSlide);
        });
    }

    private createDeck(): Promise<Deck> {
        return new Promise<Deck>(async (resolve, reject) => {
            try {
                this.authService.watch().pipe(filter((user: AuthUser) => user !== null && user !== undefined), take(1)).subscribe(async (authUser: AuthUser) => {
                    const deck: DeckData = {
                        name: `Presentation ${await Utils.getNow()}`,
                        owner_id: authUser.uid
                    };

                    const persistedDeck: Deck = await this.deckService.create(deck);
                    this.deckEditorService.next(persistedDeck);

                    await this.updateNavigation(persistedDeck);

                    resolve(persistedDeck);
                });
            } catch (err) {
                reject(err);
            }
        });
    }

    private updateDeckSlideList(deck: Deck, slide: Slide): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                if (!deck && !deck.data) {
                    reject('Missing deck to add the slide to the list');
                    return;
                }

                if (!slide || !slide.id || !slide.data) {
                    reject('Missing slide to create or update the deck');
                    return;
                }

                if (!deck.data.slides || deck.data.slides.length <= 0) {
                    deck.data.slides = [];
                }

                deck.data.slides.push(slide.id);

                const updatedDeck: Deck = await this.deckService.update(deck);
                this.deckEditorService.next(updatedDeck);

                resolve();
            } catch (err) {
                reject(err);
            }
        });
    }

    private updateNavigation(deck: Deck): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!deck || !deck.id) {
                resolve();
                return;
            }

            history.replaceState({}, `Deck edited ${deck.id}`, `/editor/${deck.id}`);

            resolve();
        });
    }

    private updateDeck(deck: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            try {
                if (!deck) {
                    resolve();
                    return;
                }

                this.busyService.deckBusy(true);

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (currentDeck: Deck) => {
                    if (!currentDeck || !currentDeck.data) {
                        resolve();
                        return;
                    }

                    const attributes: DeckAttributes = await this.getDeckAttributes(deck);
                    currentDeck.data.attributes = attributes && Object.keys(attributes).length > 0 ? attributes : null;

                    const background: string = await this.getDeckBackground(deck);
                    currentDeck.data.background = background && background !== undefined && background !== '' ? background : null;

                    const updatedDeck: Deck = await this.deckService.update(currentDeck);

                    this.deckEditorService.next(updatedDeck);

                    this.busyService.deckBusy(false);

                    resolve();
                });
            } catch (err) {
                this.errorService.error(err);
                this.busyService.deckBusy(false);
                resolve();
            }
        });
    }

    private updateDeckTitle(title: string): Promise<void> {
        return new Promise<void>(async (resolve) => {
            try {
                if (!title || title === undefined || title === '') {
                    resolve();
                    return;
                }

                this.busyService.deckBusy(true);

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (currentDeck: Deck) => {
                    if (!currentDeck || !currentDeck.data) {
                        resolve();
                        return;
                    }

                    // TODO: Add a check, we should not update the title from the slide in case it would have been set in the publication

                    if (title.length >= Resources.Constants.DECK.TITLE_MAX_LENGTH) {
                        title = title.substr(0, Resources.Constants.DECK.TITLE_MAX_LENGTH);
                    }

                    currentDeck.data.name = title;

                    const updatedDeck: Deck = await this.deckService.update(currentDeck);

                    this.deckEditorService.next(updatedDeck);

                    this.busyService.deckBusy(false);

                    resolve();
                });
            } catch (err) {
                this.errorService.error(err);
                this.busyService.deckBusy(false);
                resolve();
            }
        });
    }

    private updateSlide(slide: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            try {
                if (!slide || !slide.nodeName) {
                    resolve();
                    return;
                }

                if (!slide.getAttribute('slide_id')) {
                    this.errorService.error('Slide is not defined');
                    resolve();
                    return;
                }

                const slideUpdate: Slide = {
                    id: slide.getAttribute('slide_id'),
                    data: {
                        template: this.getSlideTemplate(slide)
                    }
                };

                const content: string = await this.cleanSlideContent(slide);
                if (content && content.length > 0) {
                    slideUpdate.data.content = content
                }

                const attributes: SlideAttributes = await this.getSlideAttributes(slide);

                if (attributes && Object.keys(attributes).length > 0) {
                    slideUpdate.data.attributes = attributes;
                }

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (deck) {
                        await this.slideService.update(deck.id, slideUpdate);
                    }

                    this.busyService.deckBusy(false);

                    resolve();
                });
            } catch (err) {
                this.errorService.error(err);
                this.busyService.deckBusy(false);
                resolve();
            }
        });
    }

    private deleteSlide(slide: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            try {
                if (!slide) {
                    resolve();
                    return;
                }

                if (!slide.getAttribute('slide_id')) {
                    this.errorService.error('Slide is not defined');
                    resolve();
                    return;
                }

                const slideId: string = slide.getAttribute('slide_id');

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (deck && deck.data) {
                        await this.slideService.delete(deck.id, slideId);

                        // TODO: Ultimately should be moved in a Cloud Function
                        // Update list of slide in the deck
                        if (deck.data.slides && deck.data.slides.indexOf(slideId) > -1) {
                            deck.data.slides.splice(deck.data.slides.indexOf(slideId), 1);

                            const updatedDeck: Deck = await this.deckService.update(deck);
                            this.deckEditorService.next(updatedDeck);
                        }
                    }

                    await this.deleteSlideElement();

                    this.busyService.deckBusy(false);

                    resolve();
                });
            } catch (err) {
                this.errorService.error(err);
                this.busyService.deckBusy(false);
                resolve();
            }
        });
    }

    private deleteSlideElement(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            await (deck as any).deleteActiveSlide();

            resolve();
        });
    }

    private getSlideAttributes(slide: HTMLElement): Promise<SlideAttributes> {
        return new Promise<SlideAttributes>((resolve) => {
            let attributes: SlideAttributes = {};

            if (slide.getAttribute('style')) {
                attributes.style = slide.getAttribute('style');
            }

            if ((slide as any).src) {
                attributes.src = (slide as any).src;
            }

            if (slide.hasAttribute('custom-background')) {
                attributes.customBackground = '' + true;
            }

            if ((slide as any).imgSrc) {
                attributes.imgSrc = (slide as any).imgSrc;
            }

            if ((slide as any).imgAlt) {
                attributes.imgAlt = (slide as any).imgAlt;
            }

            resolve(attributes);
        })
    }

    private getDeckAttributes(deck: HTMLElement): Promise<DeckAttributes> {
        return new Promise<DeckAttributes>((resolve) => {
            let attributes: DeckAttributes = {};

            if (deck.getAttribute('style')) {
                attributes.style = deck.getAttribute('style');
            }

            resolve(attributes);
        })
    }

    private getDeckBackground(deck: HTMLElement): Promise<string> {
        return new Promise<string>((resolve) => {
            const slotElement: HTMLElement = deck.querySelector(':scope > [slot=\'background\']');

            if (!slotElement) {
                resolve(null);
                return;
            }

            resolve(slotElement.innerHTML);
        });
    }

    private cleanSlideContent(slide: HTMLElement): Promise<string> {
        return new Promise<string>((resolve) => {
            if (!slide) {
                resolve(null);
                return;
            }

            const content: string = slide.innerHTML;

            if (!content || content.length <= 0) {
                resolve(content);
                return;
            }

            let result: string = content.replace(/contenteditable=""|contenteditable="true"|contenteditable="false"|contenteditable/gi, '');
            result = result.replace(/editable=""|editable="true"|editable/gi, '');
            result = result.replace(/highlighted=""|highlighted="true"|highlighted/gi, '');
            result = result.replace(/hydrated/gi, '');
            result = result.replace(/class=""/g, '');

            if (!slide.hasAttribute('custom-background')) {
                result = result.replace(/<div slot="background">(.*?)<\/div>/g, '');
            }

            resolve(result);
        });
    }

    private getSlideTemplate(slide: HTMLElement): SlideTemplate {
        const templateKey: string = Object.keys(SlideTemplate).find((key: string) => {
            return slide.nodeName.toLowerCase().indexOf(SlideTemplate[key]) > -1
        });

        return SlideTemplate[templateKey];
    }

    private slideToLastSlide(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck || !deck.children || deck.children.length <= 0) {
                resolve();
                return;
            }

            const slides: Element[] = Array.from(deck.children).filter((slide: Element) => {
                return slide.tagName.toLocaleLowerCase().indexOf('deckgo-slide-') > -1
            });

            if (!slides || slides.length <= 0) {
                resolve();
                return;
            }

            const lastSlide: Element = slides[slides.length - 1];

            if (!lastSlide || lastSlide.getAttribute('slide_id')) {
                resolve();
                return;
            }

            await (deck as any).slideTo(slides.length - 1);

            resolve();
        });
    }

    initSlideSize(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                return;
            }

            await (deck as any).initSlideSize();

            resolve();
        });
    }

    updateDeckSlidesOrder(detail: ItemReorderEventDetail): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                if (!detail) {
                    reject('No new order provided for the slides');
                    return;
                }

                if (detail.from < 0 || detail.to < 0 || detail.from === detail.to) {
                    reject('The new order provided for the slides is not valid');
                    return;
                }

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (deck && deck.data && deck.data.slides && detail.to < deck.data.slides.length) {
                        deck.data.slides.splice(detail.to, 0, ...deck.data.slides.splice(detail.from, 1));

                        const updatedDeck: Deck = await this.deckService.update(deck);
                        this.deckEditorService.next(updatedDeck);
                    }

                    resolve();
                });
            } catch (err) {
                reject(err);
            }
        });
    }
}
