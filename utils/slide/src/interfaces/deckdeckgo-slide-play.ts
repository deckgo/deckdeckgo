import {DeckdeckgoSlideResize} from './deckdeckgo-slide-resize';

export interface DeckdeckgoSlidePlay extends DeckdeckgoSlideResize {
  play(): Promise<void>;

  pause(): Promise<void>;

  toggle(): Promise<void>;
}
