import {SlideTemplate} from '../../models/slide-template';
import {Slide} from '../../models/slide';

import {SlideService} from '../../services/slide/slide.service';

export class EditorHelper {

    private el: HTMLElement;

    private slideService: SlideService;
    // private deckService: DeckService;

    constructor() {
        this.slideService = SlideService.getInstance();
        // this.deckService = DeckService.getInstance();
    }

    init(el: HTMLElement) {
        this.el = el;

        this.el.addEventListener('slideDidLoad', this.onSlideDidLoad, false);
    }

    destroy() {
        this.el.removeEventListener('change', this.onSlideDidLoad, true);
    }

    private onSlideDidLoad = async ($event: CustomEvent) => {
        if ($event && $event.target && $event.target instanceof HTMLElement) {
            await this.createSlide($event.target);
        }
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

                const persistedSlide: Slide = await this.slideService.post({
                    slide_template: SlideTemplate.TITLE,
                    slide_content: slide.innerHTML
                });

                if (persistedSlide && persistedSlide.slide_id) {
                    slide.setAttribute('slide_id', persistedSlide.slide_id);
                }
            } catch (err) {
                // TODO do something with the error
                console.error(err);
                resolve();
            }
        });
    }


}
