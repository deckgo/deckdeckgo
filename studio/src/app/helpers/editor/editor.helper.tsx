import {Subject, Subscription} from 'rxjs';
import {debounceTime} from 'rxjs/operators';

import {SlideTemplate} from '../../models/slide-template';
import {Slide} from '../../models/slide';
import {Deck} from '../../models/deck';

import {SlideService} from '../../services/slide/slide.service';
import {DeckService} from '../../services/deck/deck.service';
import {ErrorService} from '../../services/error/error.service';
import {DeckBusyService} from '../../services/deck/deck-busy.service';
import {SlideAttributes} from '../../models/slide-attributes';

export class EditorHelper {

    private el: HTMLElement;

    private slideService: SlideService;
    private deckService: DeckService;

    private errorService: ErrorService;
    private deckBusyService: DeckBusyService;

    private deck: Deck;

    private subscription: Subscription;
    private updateSlideSubject: Subject<HTMLElement> = new Subject();

    constructor() {
        this.slideService = SlideService.getInstance();
        this.deckService = DeckService.getInstance();

        this.errorService = ErrorService.getInstance();
        this.deckBusyService = DeckBusyService.getInstance();
    }

    init(el: HTMLElement) {
        this.el = el;

        this.el.addEventListener('input', this.onSlideInputChange, false);
        this.el.addEventListener('slideDidChange', this.onSlideChange, false);
        this.el.addEventListener('slideDidLoad', this.onSlideDidLoad, false);
        this.el.addEventListener('slideDelete', this.onSlideDelete, false);

        this.subscription = this.updateSlideSubject.pipe(debounceTime(500)).subscribe(async (element: HTMLElement) => {
            await this.updateSlide(element);
        });
    }

    destroy() {
        this.el.removeEventListener('input', this.onSlideInputChange, true);
        this.el.removeEventListener('slideDidChange', this.onSlideChange, true);
        this.el.removeEventListener('slideDidLoad', this.onSlideDidLoad, true);
        this.el.removeEventListener('slideDelete', this.onSlideDelete, true);

        if (this.subscription) {
            this.subscription.unsubscribe();
        }
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
                if (!slide) {
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
                    slide_template: SlideTemplate.TITLE
                };

                const content: string = await this.cleanSlideContent(slide.innerHTML);
                if (content && content.length > 0) {
                    slidePost.slide_content = content
                }

                const persistedSlide: Slide = await this.slideService.post(slidePost);

                if (persistedSlide && persistedSlide.slide_id) {
                    slide.setAttribute('slide_id', persistedSlide.slide_id);

                    await this.createOrUpdateDeck(persistedSlide);
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

    private createOrUpdateDeck(slide: Slide): Promise<void> {
        return new Promise<void>(async (resolve, reject) => {
            try {
                if (!slide) {
                    reject('Missing slide to create or update the deck');
                    return;
                }

                if (this.deck) {
                    if (!this.deck.deck_slides || this.deck.deck_slides.length <= 0) {
                        this.deck.deck_slides = [];
                    }

                    this.deck.deck_slides.push(slide.slide_id);

                    this.deck = await this.deckService.put(this.deck);
                } else {
                    this.deck = {
                        deck_slides: [slide.slide_id]
                    };

                    this.deck = await this.deckService.post(this.deck);
                }

                resolve();
            } catch (err) {
                reject(err);
            }
        });
    }

    private updateSlide(slide: HTMLElement): Promise<void> {
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

                const slideUpdate: Slide = {
                    slide_id: slide.getAttribute('slide_id'),
                    slide_template: SlideTemplate.TITLE
                };

                const content: string = await this.cleanSlideContent(slide.innerHTML);
                if (content && content.length > 0) {
                    slideUpdate.slide_content = content
                }

                const attributes: SlideAttributes = await this.getSlideAttributes(slide);

                if (attributes && Object.keys(attributes).length > 0) {
                    slideUpdate.slide_attributes = attributes;
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

                await this.slideService.delete(slide.getAttribute('slide_id'));

                await this.deleteSlideElement();

                this.deckBusyService.busy(false);

                resolve();
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

            let result: string = content.replace(/deckgo-untouched|contenteditable=""|contenteditable="true"|contenteditable/gi, '');
            result = result.replace(/class=""/g, '');
            result = result.replace(/\s\s+/g, '');

            resolve(result);
        });
    }
}
