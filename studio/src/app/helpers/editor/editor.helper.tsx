import {Slide} from '../../models/slide';

import {SlideService} from '../../services/slide/slide.service';
import {DeckService} from '../../services/deck/deck.service';
import {ErrorService} from '../../services/error/error.service';
import {DeckBusyService} from '../../services/deck/deck-busy.service';
import {Deck} from '../../models/deck';

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
                    promises.push(this.slideService.get(slideId));
                });

                // TODO load slide to be rendered

                let slides: Slide[] = [];
                if (promises.length > 0) {
                    slides = await Promise.all(promises);
                }
                //
                // const slide: any = <deckgo-slide-title>
                //    {new DOMParser().parseFromString(slides[0].slide_content, 'text/html')}
                // </deckgo-slide-title>;

                const title: any = <h1 slot="title" class="deckgo-untouched" contenteditable>
                    HELLO
                </h1>;

                // @ts-ignore
                const slide: any = <deckgo-slide-title slide_id={slides[0].id}>
                    {title}
                </deckgo-slide-title>;

                this.deckBusyService.busy(false);

                const tmp = [];
                tmp.push(slide);

                // TODO: There is a bug right now, it does a post after this!!! so we end up with 2 decks instead of 1 in the db

                resolve(tmp);
            } catch (err) {
                this.errorService.error(err);
                this.deckBusyService.busy(false);
                resolve(null);
            }
        });
    }

}
