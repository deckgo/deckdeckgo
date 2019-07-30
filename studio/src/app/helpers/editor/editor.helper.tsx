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
                    promises.push(this.fetchSlide(deckId, slideId));
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

    private fetchSlide(deckId: string, slideId: string): Promise<any> {
        return new Promise<any>(async (resolve) => {
            try {
                const slide: Slide = await this.slideService.get(deckId, slideId);
                const element: any = await ParseSlidesUtils.parseSlide(slide, true);

                resolve(element);
            } catch (err) {
                this.errorService.error('Something went wrong while loading and parsing a slide');
                resolve();
            }
        });
    }

}
