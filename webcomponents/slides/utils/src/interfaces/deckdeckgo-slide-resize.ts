import {DeckdeckgoSlide} from './deckdeckgo-slide';

export interface DeckdeckgoSlideResize extends DeckdeckgoSlide {
    resizeContent(): Promise<void>;
}
