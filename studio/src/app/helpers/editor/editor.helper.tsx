import {ApiSlide} from '../../models/api/api.slide';

import {ApiDeck} from '../../models/api/api.deck';

import {ParseSlidesUtils} from '../../utils/editor/parse-slides.utils';

import {ApiSlideService} from '../../services/api/slide/api.slide.service';
import {ApiDeckService} from '../../services/api/deck/api.deck.service';
import {ErrorService} from '../../services/core/error/error.service';
import {BusyService} from '../../services/editor/busy/busy.service';
import {DeckEditorService} from '../../services/editor/deck/deck-editor.service';

export class EditorHelper {

    private slideService: ApiSlideService;
    private deckService: ApiDeckService;

    private errorService: ErrorService;
    private busyService: BusyService;

    private deckEditorService: DeckEditorService;

    constructor() {
        this.slideService = ApiSlideService.getInstance();
        this.deckService = ApiDeckService.getInstance();

        this.errorService = ErrorService.getInstance();
        this.busyService = BusyService.getInstance();

        this.deckEditorService = DeckEditorService.getInstance();
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
                const deck: ApiDeck = await this.deckService.get(deckId);

                if (!deck) {
                    this.errorService.error('No deck could be fetched');
                    resolve(null);
                    return;
                }

                this.deckEditorService.next(deck);

                if (!deck.slides || deck.slides.length <= 0) {
                    resolve([]);
                    return;
                }

                const promises: Promise<ApiSlide>[] = [];
                deck.slides.forEach((slideId: string) => {
                    promises.push(this.fetchSlide(deck.id, slideId));
                });

                let slides: ApiSlide[] = [];
                if (promises.length > 0) {
                    slides = await Promise.all(promises);
                }

                if (!slides || slides.length <= 0) {
                    resolve([]);
                    return;
                }

                this.busyService.deckBusy(false);

                resolve(slides);
            } catch (err) {
                this.errorService.error(err);
                this.busyService.deckBusy(false);
                resolve(null);
            }
        });
    }

    private fetchSlide(deckId: string, slideId: string): Promise<any> {
        return new Promise<any>(async (resolve) => {
            try {
                const slide: ApiSlide = await this.slideService.get(deckId, slideId);
                const element: any = await ParseSlidesUtils.parseSlide(slide);

                resolve(element);
            } catch (err) {
                this.errorService.error('Something went wrong while loading and parsing a slide');
                resolve();
            }
        });
    }

}
