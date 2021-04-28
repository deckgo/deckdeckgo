import {Component, Element, Event, EventEmitter, Method, h, Host, Prop, State} from '@stencil/core';

import {debounce, isFullscreen} from '@deckdeckgo/utils';
import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

/**
 * @slot - Multiple elements to be displayed in the slide
 * @slot top - An optional text to be displayed at the top
 * @slot bottom - An optional text to be displayed at the bottom
 * @slot code - A block of code to highlight if src is not used
 * @slot notes - Some notes related to this slide
 * @slot actions - Custom actions for this slide
 * @slot background - A custom background for this slide
 * @slot header - A custom header for this slide
 * @slot footer - A custom footer for this slide
 */
@Component({
  tag: 'deckgo-slide-aspect-ratio',
  styleUrl: 'deckdeckgo-slide-aspect-ratio.scss',
  shadow: true,
})
export class DeckdeckgoSlideAspectRatio implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  /**
   * Triggered when the slide is loaded
   */
  @Event() slideDidLoad: EventEmitter<void>;

  /**
   * The aspect ratio of the displayed content. Per default 16 being the width and 9 the height
   */
  @Prop()
  ratio: number = 16 / 9;

  /**
   * Display a grid behind the content. Note that the grid would only be display if not fullscreen
   */
  @Prop()
  grid: boolean = false;

  /**
   * Per default point-events are set to none for this template making it read-only respectively not editable
   */
  @Prop()
  editable: boolean = false;

  /**
   * If you define a background for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customBackground: boolean = false;

  /**
   * If you provide actions for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customActions: boolean = false;

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
