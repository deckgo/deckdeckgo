import {Component, Element, Event, EventEmitter, Method, Prop, Listen, State, h, Host} from '@stencil/core';

import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

/**
 * @slot title - A title
 * @slot top - An element to display at the top of the Gif
 * @slot bottom - An element to display at the bottom of the Gif
 * @slot notes - Some notes related to this slide
 * @slot actions - Custom actions for this slide
 * @slot background - A custom background for this slide
 * @slot header - A custom header for this slide
 * @slot footer - A custom footer for this slide
 */
@Component({
  tag: 'deckgo-slide-gif',
  styleUrl: 'deckdeckgo-slide-gif.scss',
  shadow: true
})
export class DeckdeckgoSlideGif implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  /**
   * Triggered when the slide is loaded
   */
  @Event()
  slideDidLoad: EventEmitter<void>;

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

  /**
   * The src (url) of the Gif
   */
  @Prop({reflect: true})
  src: string;

  /**
   * An alternate text for the Gif
   */
  @Prop({reflect: true})
  alt: string;

  /**
   * If set to true, the GIF width and height will be related to the slide width and height respectively will be fullscreen.
   */
  @Prop()
  fullscreen: boolean = true;

  @State() loaded: boolean = false;

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();
  }

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

  @Listen('gifLoaded')
  onGifLoaded($event: CustomEvent) {
    this.loaded = $event && $event.detail;
  }

  render() {
    return (
      <Host
        class={{
          'deckgo-slide-container': true,
          'deckgo-slide-container-fullscreen': this.fullscreen,
          'deckgo-slide-gif-hidden': !this.loaded
        }}>
        <div class="deckgo-slide">
          <slot name="title"></slot>
          <div class="deckgo-gif-container">
            <slot name="top"></slot>
            <deckgo-gif src={this.src} alt={this.alt} fullscreen={this.fullscreen}></deckgo-gif>
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
