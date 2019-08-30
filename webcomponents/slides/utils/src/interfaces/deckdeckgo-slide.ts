export interface DeckdeckgoSlide {
  beforeSwipe(enter: boolean, _reveal: boolean): Promise<boolean>;

  afterSwipe(): Promise<void>;

  lazyLoadContent(): Promise<void>;

  revealContent(): Promise<void>;

  hideContent(): Promise<void>;
}
