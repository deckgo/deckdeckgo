import {Component, Element, Event, EventEmitter, Method, h, Host, Prop, State} from '@stencil/core';

import {debounce, isFullscreen} from '@deckdeckgo/utils';
import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-slide-aspect-ratio',
  styleUrl: 'deckdeckgo-slide-aspect-ratio.scss',
  shadow: true,
})
export class DeckdeckgoSlideAspectRatio implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop()
  ratio: number = 16 / 9;

  @Prop()
  grid: boolean = false;

  @Prop()
  editable: boolean = false;

  @State()
  private displayGrid: boolean = false;

  refContainer!: HTMLDivElement;

  async componentWillLoad() {
    this.displayGrid = this.grid;
  }

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.initWindowResize();

    this.slideDidLoad.emit();
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onResizeContent));
    }
  }

  private onResizeContent = async () => {
    this.displayGrid = this.grid && !isFullscreen();
  };

  @Method()
  beforeSwipe(_enter: boolean, _reveal: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true);
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return lazyLoadContent(this.el);
  }

  @Method()
  revealContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  hideContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  async getContainer(): Promise<HTMLDivElement> {
    return this.refContainer;
  }

  render() {
    let classContent: string = 'deckgo-aspect-ratio-content';

    if (this.displayGrid) {
      classContent += ' deckgo-aspect-ratio-content-grid';
    }

    return (
      <Host
        class={{
          'deckgo-slide-container': true,
          'deckgo-read-only': !this.editable,
        }}>
        <div class="deckgo-slide">
          <slot name="title"></slot>
          <div class="deckgo-aspect-ratio-container">
            <div class={classContent} ref={(el) => (this.refContainer = el as HTMLDivElement)}>
              <slot />
            </div>
            <slot name="top"></slot>
            <slot name="bottom"></slot>
          </div>
          <slot name="notes"></slot>
          <slot name="actions"></slot>
          <slot name="background"></slot>
          <slot name="header"></slot>
          <slot name="footer"></slot>
        </div>
      </Host>
    );
  }
}
