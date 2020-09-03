import {Component, Element, Event, EventEmitter, Method, Prop, Listen, State, h, Host} from '@stencil/core';

import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-slide-gif',
  styleUrl: 'deckdeckgo-slide-gif.scss',
  shadow: true,
})
export class DeckdeckgoSlideGif implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({reflect: true}) src: string;
  @Prop({reflect: true}) alt: string;

  @Prop() fullscreen: boolean = true;

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
          'deckgo-slide-gif-hidden': !this.loaded,
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
