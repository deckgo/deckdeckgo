import {Slide} from '../../models/slide';

import {Deck} from '../../models/deck';

import {ParseSlidesUtils} from '../../utils/editor/parse-slides.utils';

import {SlideService} from '../../services/slide/slide.service';
import {DeckService} from '../../services/deck/deck.service';
import {ErrorService} from '../../services/error/error.service';
import {DeckBusyService} from '../../services/deck/deck-busy.service';

export class EditorHelper {

    private slideService: SlideService;
    private deckService: DeckService;

    private errorService: ErrorService;
    private deckBusyService: DeckBusyService;

    constructor() {
        this.slideService = SlideService.getInstance();
        this.deckService = DeckService.getInstance();

        this.errorService = ErrorService.getInstance();
        this.deckBusyService = DeckBusyService.getInstance();
    }

    retrieveSlides(deckId: string): Promise<any[]> {
        return new Promise<any[]>(async (resolve) => {
            if (!deckId) {
                this.errorService.error('Deck is not defined');
                resolve(null);
                return;
            }

            this.deckBusyService.busy(true);

            try {
                const deck: Deck = await this.deckService.get(deckId);

                if (!deck) {
                    this.errorService.error('No deck could be fetched');
                    resolve(null);
                    return;
                }

                if (!deck.slides || deck.slides.length <= 0) {
                    resolve([]);
                    return;
                }

                const promises: Promise<Slide>[] = [];
                deck.slides.forEach((slideId: string) => {
                    promises.push(this.fetchSlide(slideId));
                });

                let slides: Slide[] = [];
                if (promises.length > 0) {
                    slides = await Promise.all(promises);
                }

                if (!slides || slides.length <= 0) {
                    resolve([]);
                    return;
                }

                this.deckBusyService.busy(false);

                resolve(slides);
            } catch (err) {
                this.errorService.error(err);
                this.deckBusyService.busy(false);
                resolve(null);
            }
        });
    }

    private fetchSlide(slideId: string): Promise<any> {
        return new Promise<any>(async (resolve, reject) => {
            try {
                const slide: Slide = await this.slideService.get(slideId);
                const element: any = await ParseSlidesUtils.parseSlide(slide);

                resolve(element);
            } catch (err) {
                reject(err);
            }
        });
    }

}
