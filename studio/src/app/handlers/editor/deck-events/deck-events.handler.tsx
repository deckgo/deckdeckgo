import {Subject, Subscription} from 'rxjs';
import {debounceTime, filter, take} from 'rxjs/operators';

import {Slide, SlideAttributes, SlideTemplate} from '../../../models/slide';
import {User} from '../../../models/user';
import {Deck} from '../../../models/deck';

import {Utils} from '../../../utils/core/utils';

import {SlideService} from '../../../services/slide/slide.service';
import {DeckService} from '../../../services/deck/deck.service';
import {ErrorService} from '../../../services/error/error.service';
import {DeckBusyService} from '../../../services/deck/deck-busy.service';
import {UserService} from '../../../services/user/user.service';
import {DeckEditorService} from '../../../services/deck/deck-editor.service';

export class DeckEventsHandler {

    private el: HTMLElement;

    private slideService: SlideService;
    private deckService: DeckService;

    private errorService: ErrorService;
    private deckBusyService: DeckBusyService;

    private userSubscription: Subscription;
    private userService: UserService;

    private updateSlideSubscription: Subscription;
    private updateSlideSubject: Subject<HTMLElement> = new Subject();

    private deckEditorService: DeckEditorService;

    constructor() {
        this.slideService = SlideService.getInstance();
        this.deckService = DeckService.getInstance();

        this.errorService = ErrorService.getInstance();
        this.deckBusyService = DeckBusyService.getInstance();

        this.userService = UserService.getInstance();

        this.deckEditorService = DeckEditorService.getInstance();
    }

    init(el: HTMLElement) {
        this.el = el;

        this.el.addEventListener('input', this.onSlideInputChange, false);
        this.el.addEventListener('slideDidChange', this.onSlideChange, false);
        this.el.addEventListener('slideDidLoad', this.onSlideDidLoad, false);
        this.el.addEventListener('slideDelete', this.onSlideDelete, false);

        this.updateSlideSubscription = this.updateSlideSubject.pipe(debounceTime(500)).subscribe(async (element: HTMLElement) => {
            await this.updateSlide(element);
        });
    }

    destroy() {
        this.el.removeEventListener('input', this.onSlideInputChange, true);
        this.el.removeEventListener('slideDidChange', this.onSlideChange, true);
        this.el.removeEventListener('slideDidLoad', this.onSlideDidLoad, true);
        this.el.removeEventListener('slideDelete', this.onSlideDelete, true);

        if (this.updateSlideSubscription) {
            this.updateSlideSubscription.unsubscribe();
        }

        if (this.userSubscription) {
            this.userSubscription.unsubscribe();
        }

        this.deckEditorService.next(null);
    }

    private onSlideDidLoad = async ($event: CustomEvent) => {
        if ($event && $event.target && $event.target instanceof HTMLElement) {
            await this.createSlide($event.target);
        }
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

        const parent: HTMLElement = ($event.target as HTMLElement).parentElement;

        if (!parent || !parent.nodeName || parent.nodeName.toLowerCase().indexOf('deckgo-slide') <= -1) {
            return;
        }

        this.updateSlideSubject.next(parent);
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
                    resolve();
                    return;
                }

                this.deckBusyService.busy(true);

                const slidePost: Slide = {
                    template: this.getSlideTemplate(slide)
                };

                const content: string = await this.cleanSlideContent(slide.innerHTML);
                if (content && content.length > 0) {
                    slidePost.content = content
                }

                const persistedSlide: Slide = await this.slideService.post(slidePost);

                if (persistedSlide && persistedSlide.id) {
                    slide.setAttribute('slide_id', persistedSlide.id);

                    await this.createOrUpdateDeckSlideList(persistedSlide);
                }

                this.deckBusyService.busy(false);

                resolve();
            } catch (err) {
                this.errorService.error(err);
                this.deckBusyService.busy(false);
                resolve();
            }
        });
    }

    private createOrUpdateDeckSlideList(slide: Slide): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                if (!slide) {
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
                    } else {
                        this.userService.watch().pipe(filter((user: User) => user !== null && user !== undefined), take(1)).subscribe(async (user: User) => {
                            deck = {
                                slides: [slide.id],
                                name: `Presentation ${await Utils.getNow()}`,
                                owner_id: user.id
                            };

                            const persistedDeck: Deck = await this.deckService.post(deck);
                            this.deckEditorService.next(persistedDeck);

                            await this.updateNavigation(persistedDeck);
                        });
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

                await this.slideService.put(slideUpdate);

                this.deckBusyService.busy(false);

                resolve();
            } catch (err) {
                this.errorService.error(err);
                this.deckBusyService.busy(false);
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

                await this.slideService.delete(slideId);

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    // Update list of slide in the deck
                    if (deck && deck.slides && deck.slides.indexOf(slideId) > -1) {
                        deck.slides.splice(deck.slides.indexOf(slideId), 1);

                        const updatedDeck: Deck = await this.deckService.put(deck);
                        this.deckEditorService.next(updatedDeck);
                    }

                    await this.deleteSlideElement();

                    this.deckBusyService.busy(false);

                    resolve();
                });
            } catch (err) {
                this.errorService.error(err);
                this.deckBusyService.busy(false);
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
}
