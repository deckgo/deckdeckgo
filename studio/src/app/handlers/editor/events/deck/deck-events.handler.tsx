import {Subject, Subscription} from 'rxjs';
import {debounceTime, filter, take} from 'rxjs/operators';

import {ApiSlide, ApiSlideAttributes, ApiSlideTemplate} from '../../../../models/api/api.slide';
import {ApiUser} from '../../../../models/api/api.user';
import {ApiDeck, ApiDeckAttributes} from '../../../../models/api/api.deck';

import {Utils} from '../../../../utils/core/utils';
import {Resources} from '../../../../utils/core/resources';

import {ApiSlideService} from '../../../../services/api/slide/api.slide.service';
import {ApiDeckService} from '../../../../services/api/deck/api.deck.service';
import {ErrorService} from '../../../../services/core/error/error.service';
import {BusyService} from '../../../../services/editor/busy/busy.service';
import {ApiUserService} from '../../../../services/api/user/api.user.service';
import {DeckEditorService} from '../../../../services/editor/deck/deck-editor.service';

export class DeckEventsHandler {

    private el: HTMLElement;

    private slideService: ApiSlideService;
    private deckService: ApiDeckService;

    private errorService: ErrorService;
    private busyService: BusyService;

    private userService: ApiUserService;

    private updateSlideSubscription: Subscription;
    private updateSlideSubject: Subject<HTMLElement> = new Subject();

    private deckEditorService: DeckEditorService;

    private updateDeckTitleSubscription: Subscription;
    private updateDeckTitleSubject: Subject<string> = new Subject();

    constructor() {
        this.slideService = ApiSlideService.getInstance();
        this.deckService = ApiDeckService.getInstance();

        this.errorService = ErrorService.getInstance();
        this.busyService = BusyService.getInstance();

        this.userService = ApiUserService.getInstance();

        this.deckEditorService = DeckEditorService.getInstance();
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

        const parent: HTMLElement = element.parentElement;

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

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: ApiDeck) => {
                    if (!deck) {
                        deck = await this.createDeck();
                    }

                    const persistedSlide: ApiSlide = await this.postSlide(deck, slide);

                    await this.updateDeckSlideList(persistedSlide);

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

    private postSlide(deck: ApiDeck, slide: HTMLElement): Promise<ApiSlide> {
        return new Promise<ApiSlide>(async (resolve) => {
            const slidePost: ApiSlide = {
                template: this.getSlideTemplate(slide)
            };

            const content: string = await this.cleanSlideContent(slide);
            if (content && content.length > 0) {
                slidePost.content = content
            }

            const attributes: ApiSlideAttributes = await this.getSlideAttributes(slide);

            if (attributes && Object.keys(attributes).length > 0) {
                slidePost.attributes = attributes;
            }

            const persistedSlide: ApiSlide = await this.slideService.post(deck.id, slidePost);

            if (persistedSlide && persistedSlide.id) {
                slide.setAttribute('slide_id', persistedSlide.id);

                this.busyService.slideEditable(slide);
            }

            resolve(persistedSlide);
        });
    }

    private createDeck(): Promise<ApiDeck> {
        return new Promise<ApiDeck>(async (resolve, reject) => {
            try {
                this.userService.watch().pipe(filter((user: ApiUser) => user !== null && user !== undefined), take(1)).subscribe(async (user: ApiUser) => {
                    const deck: ApiDeck = {
                        slides: [],
                        name: `Presentation ${await Utils.getNow()}`,
                        owner_id: user.id
                    };

                    const persistedDeck: ApiDeck = await this.deckService.post(deck);
                    this.deckEditorService.next(persistedDeck);

                    await this.updateNavigation(persistedDeck);

                    resolve(persistedDeck);
                });
            } catch (err) {
                reject(err);
            }
        });
    }

    private updateDeckSlideList(slide: ApiSlide): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                if (!slide || !slide.id) {
                    reject('Missing slide to create or update the deck');
                    return;
                }

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: ApiDeck) => {
                    if (deck) {
                        if (!deck.slides || deck.slides.length <= 0) {
                            deck.slides = [];
                        }

                        deck.slides.push(slide.id);

                        const updatedDeck: ApiDeck = await this.deckService.put(deck);
                        this.deckEditorService.next(updatedDeck);
                    }

                    resolve();
                });
            } catch (err) {
                reject(err);
            }
        });
    }

    private updateNavigation(deck: ApiDeck): Promise<void> {
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

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (currentDeck: ApiDeck) => {
                    if (!currentDeck) {
                        resolve();
                        return;
                    }

                    const attributes: ApiDeckAttributes = await this.getDeckAttributes(deck);
                    currentDeck.attributes = attributes && Object.keys(attributes).length > 0 ? attributes : null;

                    const background: string = await this.getDeckBackground(deck);
                    currentDeck.background = background && background !== undefined && background !== '' ? background : null;

                    const updatedDeck: ApiDeck = await this.deckService.put(currentDeck);
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

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (currentDeck: ApiDeck) => {
                    if (!currentDeck) {
                        resolve();
                        return;
                    }

                    // TODO: Add a check, we should not update the title from the slide in case it would have been set in the publication

                    if (title.length >= Resources.Constants.DECK.TITLE_MAX_LENGTH) {
                        title = title.substr(0, Resources.Constants.DECK.TITLE_MAX_LENGTH);
                    }

                    currentDeck.name = title;

                    const updatedDeck: ApiDeck = await this.deckService.put(currentDeck);
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

                const slideUpdate: ApiSlide = {
                    id: slide.getAttribute('slide_id'),
                    template: this.getSlideTemplate(slide)
                };

                const content: string = await this.cleanSlideContent(slide);
                if (content && content.length > 0) {
                    slideUpdate.content = content
                }

                const attributes: ApiSlideAttributes = await this.getSlideAttributes(slide);

                if (attributes && Object.keys(attributes).length > 0) {
                    slideUpdate.attributes = attributes;
                }

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: ApiDeck) => {
                    if (deck) {
                        await this.slideService.put(deck.id, slideUpdate);
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

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: ApiDeck) => {
                    if (deck) {
                        await this.slideService.delete(deck.id, slideId);

                        // Update list of slide in the deck
                        if (deck.slides && deck.slides.indexOf(slideId) > -1) {
                            deck.slides.splice(deck.slides.indexOf(slideId), 1);

                            const updatedDeck: ApiDeck = await this.deckService.put(deck);
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

    private getSlideAttributes(slide: HTMLElement): Promise<ApiSlideAttributes> {
        return new Promise<ApiSlideAttributes>((resolve) => {
            let attributes: ApiSlideAttributes = {};

            if (slide.getAttribute('style')) {
                attributes.style = slide.getAttribute('style');
            }

            if ((slide as any).src) {
                attributes.src = (slide as any).src;
            }

            if (slide.hasAttribute('custom-background')) {
                attributes.customBackground = '' + true;
            }

            resolve(attributes);
        })
    }

    private getDeckAttributes(deck: HTMLElement): Promise<ApiDeckAttributes> {
        return new Promise<ApiDeckAttributes>((resolve) => {
            let attributes: ApiDeckAttributes = {};

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
            result = result.replace(/hydrated/gi, '');
            result = result.replace(/class=""/g, '');

            if (!slide.hasAttribute('custom-background')) {
                result = result.replace(/<div slot="background">(.*?)<\/div>/g, '');
            }

            resolve(result);
        });
    }

    private getSlideTemplate(slide: HTMLElement): ApiSlideTemplate {
        const templateKey: string = Object.keys(ApiSlideTemplate).find((key: string) => {
            return slide.nodeName.toLowerCase().indexOf(ApiSlideTemplate[key]) > -1
        });

        return ApiSlideTemplate[templateKey];
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
}
