import {JSX} from '@stencil/core';
import {take} from 'rxjs/operators';

import {Slide} from '../../models/data/slide';
import {Deck} from '../../models/data/deck';

import {ParseSlidesUtils} from '../../utils/editor/parse-slides.utils';

import {ErrorService} from '../../services/core/error/error.service';
import {BusyService} from '../../services/editor/busy/busy.service';
import {DeckEditorService} from '../../services/editor/deck/deck-editor.service';
import {DeckService} from '../../services/data/deck/deck.service';
import {SlideService} from '../../services/data/slide/slide.service';

export class EditorHelper {

    private errorService: ErrorService;
    private busyService: BusyService;

    private deckEditorService: DeckEditorService;

    private slideService: SlideService;
    private deckService: DeckService;

    constructor() {
        this.slideService = SlideService.getInstance();

        this.errorService = ErrorService.getInstance();
        this.busyService = BusyService.getInstance();

        this.deckEditorService = DeckEditorService.getInstance();

        this.deckService = DeckService.getInstance();
    }

    loadDeckAndRetrieveSlides(deckId: string): Promise<any[]> {
        return new Promise<any[]>(async (resolve) => {
            if (!deckId) {
                this.errorService.error('Deck is not defined');
                resolve(null);
                return;
            }

            this.busyService.deckBusy(true);

            try {
                const deck: Deck = await this.deckService.get(deckId);

                if (!deck || !deck.data) {
                    this.errorService.error('No deck could be fetched');
                    resolve(null);
                    return;
                }

                this.deckEditorService.next(deck);

                if (!deck.data.slides || deck.data.slides.length <= 0) {
                    resolve([]);
                    return;
                }

                const promises: Promise<any>[] = [];
                deck.data.slides.forEach((slideId: string) => {
                    promises.push(this.fetchSlide(deck, slideId));
                });

                let parsedSlides: any[] = [];
                if (promises.length > 0) {
                    parsedSlides = await Promise.all(promises);
                }

                if (!parsedSlides || parsedSlides.length <= 0) {
                    resolve([]);
                    return;
                }

                this.busyService.deckBusy(false);

                resolve(parsedSlides);
            } catch (err) {
                this.errorService.error(err);
                this.busyService.deckBusy(false);
                resolve(null);
            }
        });
    }

    private fetchSlide(deck: Deck, slideId: string): Promise<JSX.IntrinsicElements> {
        return new Promise<any>(async (resolve) => {
            try {
                const slide: Slide = await this.slideService.get(deck.id, slideId);
                const element: JSX.IntrinsicElements = await ParseSlidesUtils.parseSlide(deck, slide, true);

                resolve(element);
            } catch (err) {
                this.errorService.error('Something went wrong while loading and parsing a slide');
                resolve(null);
            }
        });
    }

    copySlide(slide: HTMLElement): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>(async (resolve) => {
            try {
                if (!slide) {
                    resolve(null);
                    return;
                }

                if (!slide.getAttribute('slide_id')) {
                    this.errorService.error('Slide is not defined');
                    resolve(null);
                    return;
                }

                const slideId: string = slide.getAttribute('slide_id');

                this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                    let element: JSX.IntrinsicElements = null;

                    if (deck && deck.data) {
                        const slide: Slide = await this.slideService.get(deck.id, slideId);
                        element = await ParseSlidesUtils.parseSlide(deck, slide, true, true);
                    }

                    this.busyService.deckBusy(false);

                    resolve(element);
                });
            } catch (err) {
                this.errorService.error(err);
                this.busyService.deckBusy(false);
                resolve(null);
            }
        });
    }

}
