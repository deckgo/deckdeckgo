import {DeckdeckgoSlide} from './deckdeckgo-slide';

export interface DeckdeckgoSlidePlay extends DeckdeckgoSlide {
    play(): Promise<void>;

    pause(): Promise<void>;

    toggle(): Promise<void>;
}
