import {Component, Element, Event, EventEmitter, Method, h, Host, Prop} from '@stencil/core';

import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-slide-aspect-ratio',
  styleUrl: 'deckdeckgo-slide-aspect-ratio.scss',
  shadow: true
})
export class DeckdeckgoSlideAspectRatio implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop()
  ratio: number = 16 / 9;

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

  render() {
    return (
      <Host
        class={{
          'deckgo-slide-container': true
        }}>
        <div class="deckgo-slide">
          <slot name="title"></slot>
          <div class="deckgo-aspect-ratio-container">
            <div class="deckgo-aspect-ratio-content">
              <slot />
            </div>
            <slot name="header"></slot>
            <slot name="footer"></slot>
          </div>
          <slot name="notes"></slot>
          <slot name="actions"></slot>
          <slot name="background"></slot>
        </div>
      </Host>
    );
  }
}
