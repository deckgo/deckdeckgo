import {Subject, Subscription} from 'rxjs';
import {debounceTime, filter, take} from 'rxjs/operators';

import {Slide, SlideAttributes, SlideTemplate} from '../../../../models/slide';
import {User} from '../../../../models/user';
import {Deck, DeckAttributes} from '../../../../models/deck';

import {Utils} from '../../../../utils/core/utils';
import {Resources} from '../../../../utils/core/resources';

import {SlideService} from '../../../../services/api/slide/slide.service';
import {DeckService} from '../../../../services/api/deck/deck.service';
import {ErrorService} from '../../../../services/core/error/error.service';
import {BusyService} from '../../../../services/editor/busy/busy.service';
import {UserService} from '../../../../services/api/user/user.service';
import {DeckEditorService} from '../../../../services/editor/deck/deck-editor.service';

export class DeckEventsHandler {

    private el: HTMLElement;

    private slideService: SlideService;
    private deckService: DeckService;

    private errorService: ErrorService;
    private busyService: BusyService;

    private userService: UserService;

    private updateSlideSubscription: Subscription;
    private updateSlideSubject: Subject<HTMLElement> = new Subject();

    private deckEditorService: DeckEditorService;

    private updateDeckTitleSubscription: Subscription;
    private updateDeckTitleSubject: Subject<string> = new Subject();

    constructor() {
        this.slideService = SlideService.getInstance();
        this.deckService = DeckService.getInstance();

        this.errorService = ErrorService.getInstance();
        this.busyService = BusyService.getInstance();

        this.userService = UserService.getInstance();

        this.deckEditorService = DeckEditorService.getInstance();
    }

    init(el: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.el = el;

            this.el.addEventListener('input', this.onSlideInputChange, false);
            this.el.addEventListener('deckDidChange', this.onDeckChange, false);
            this.el.addEventListener('slideDidChange', this.onSlideChange, false);
            this.el.addEventListener('slideDidLoad', this.onSlideDidLoad, false);
            this.el.addEventListener('slideDelete', this.onSlideDelete, false);

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
        this.el.removeEventListener('input', this.onSlideInputChange, true);
        this.el.removeEventListener('deckDidChange', this.onDeckChange, true);
        this.el.removeEventListener('slideDidChange', this.onSlideChange, true);
        this.el.removeEventListener('slideDidLoad', this.onSlideDidLoad, true);
        this.el.removeEventListener('slideDelete', this.onSlideDelete, true);

        if (this.updateSlideSubscription) {
            this.updateSlideSubscription.unsubscribe();
        }

        if (this.updateDeckTitleSubscription) {
            this.updateDeckTitleSubscription.unsubscribe();
        }

        this.deckEditorService.next(null);
    }

    private onSlideDidLoad = async ($event: CustomEvent) => {
        if ($event && $event.target && $event.target instanceof HTMLElement) {
            await this.slideToLastSlide($event.target);
            await this.createSlide($event.target);
        }
    };

    private onDeckChange = async ($event: CustomEvent) => {
        if (!$event || !$event.detail) {
            return;
        }

        await this.updateDeckAttributes($event.detail);
    };

    private onSlideChange = async ($event: CustomEvent) => {
        if (!$event || !$event.detail) {
            return;
        }

        this.updateSlideSubject.next($event.detail);
    };

    private onSlideInputChange = async ($event: Event) => {
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

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (!deck) {
                        deck = await this.createDeck();
                    }

                    const persistedSlide: Slide = await this.postSlide(deck, slide);

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

    private postSlide(deck: Deck, slide: HTMLElement): Promise<Slide> {
        return new Promise<Slide>(async (resolve) => {
            const slidePost: Slide = {
                template: this.getSlideTemplate(slide)
            };

            const content: string = await this.cleanSlideContent(slide.innerHTML);
            if (content && content.length > 0) {
                slidePost.content = content
            }

            const persistedSlide: Slide = await this.slideService.post(deck.id, slidePost);

            if (persistedSlide && persistedSlide.id) {
                slide.setAttribute('slide_id', persistedSlide.id);

                this.busyService.slideEditable(slide);
            }

            resolve(persistedSlide);
        });
    }

    private createDeck(): Promise<Deck> {
        return new Promise<Deck>(async (resolve, reject) => {
            try {
                this.userService.watch().pipe(filter((user: User) => user !== null && user !== undefined), take(1)).subscribe(async (user: User) => {
                    const deck: Deck = {
                        slides: [],
                        name: `Presentation ${await Utils.getNow()}`,
                        owner_id: user.id
                    };

                    const persistedDeck: Deck = await this.deckService.post(deck);
                    this.deckEditorService.next(persistedDeck);

                    await this.updateNavigation(persistedDeck);

                    resolve(persistedDeck);
                });
            } catch (err) {
                reject(err);
            }
        });
    }

    private updateDeckSlideList(slide: Slide): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                if (!slide || !slide.id) {
                    reject('Missing slide to create or update the deck');
                    return;
                }

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (deck) {
                        if (!deck.slides || deck.slides.length <= 0) {
                            deck.slides = [];
                        }

                        deck.slides.push(slide.id);

                        const updatedDeck: Deck = await this.deckService.put(deck);
                        this.deckEditorService.next(updatedDeck);
                    }

                    resolve();
                });
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

    private updateDeckAttributes(deck: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            try {
                if (!deck) {
                    resolve();
                    return;
                }

                this.busyService.deckBusy(true);

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (currentDeck: Deck) => {
                    if (!currentDeck) {
                        resolve();
                        return;
                    }

                    const attributes: DeckAttributes = await this.getDeckAttributes(deck);

                    if (attributes && Object.keys(attributes).length > 0) {
                        currentDeck.attributes = attributes;
                    } else {
                        currentDeck.attributes = null;
                    }

                    const updatedDeck: Deck = await this.deckService.put(currentDeck);
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
                    if (!currentDeck) {
                        resolve();
                        return;
                    }

                    // TODO: Add a check, we should not update the title from the slide in case it would have been set in the publication

                    if (title.length >= Resources.Constants.DECK.TITLE_MAX_LENGTH) {
                        title = title.substr(0, Resources.Constants.DECK.TITLE_MAX_LENGTH);
                    }

                    currentDeck.name = title;

                    const updatedDeck: Deck = await this.deckService.put(currentDeck);
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
                    template: this.getSlideTemplate(slide)
                };

                const content: string = await this.cleanSlideContent(slide.innerHTML);
                if (content && content.length > 0) {
                    slideUpdate.content = content
                }

                const attributes: SlideAttributes = await this.getSlideAttributes(slide);

                if (attributes && Object.keys(attributes).length > 0) {
                    slideUpdate.attributes = attributes;
                }

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
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

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    if (deck) {
                        await this.slideService.delete(deck.id, slideId);

                        // Update list of slide in the deck
                        if (deck.slides && deck.slides.indexOf(slideId) > -1) {
                            deck.slides.splice(deck.slides.indexOf(slideId), 1);

                            const updatedDeck: Deck = await this.deckService.put(deck);
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

    private cleanSlideContent(content: string): Promise<string> {
        return new Promise<string>((resolve) => {
            if (!content || content.length <= 0) {
                resolve(content);
                return;
            }

            // TODO: Should be cleaned on publish: deckgo-untouched|
            let result: string = content.replace(/contenteditable=""|contenteditable="true"|contenteditable/gi, '');
            result = result.replace(/class=""/g, '');
            result = result.replace(/\s\s+/g, '');

            resolve(result);
        });
    }

    private getSlideTemplate(slide: HTMLElement): SlideTemplate {
        const templateKey: string = Object.keys(SlideTemplate).find((key: string) => {
            return slide.nodeName.toLowerCase().indexOf(SlideTemplate[key]) > -1
        });

        return SlideTemplate[templateKey];
    }

    private slideToLastSlide(newSlide: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            if (!newSlide.getAttribute('slide_id') && deck.hasChildNodes()) {
                await (deck as any).slideTo(deck.children && deck.children.length > 0 ? deck.children.length - 1 : 0);
            }

            resolve();
        });
    }
}
