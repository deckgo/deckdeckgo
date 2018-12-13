import {Component, Element, Event, EventEmitter, Method, Prop} from '@stencil/core';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';

@Component({
  tag: 'deckgo-slide-gif',
  styleUrl: 'deckdeckgo-slide-gif.scss',
  shadow: true
})
export class DeckdeckgoSlideGif implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() src: string;
  @Prop() alt: string;

  @Prop() fullscreen: boolean = false;

  async componentDidLoad() {
    await DeckdeckgoUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();

    await this.moveSlots();
  }

  @Method()
  beforeSwipe(_swipeLeft: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true)
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return DeckdeckgoSlideUtils.afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return DeckdeckgoSlideUtils.lazyLoadContent(this.el);
  }

  private moveSlots(): Promise<void> {
    return new Promise<void>((resolve) => {
      const header: HTMLElement = this.el.querySelector('[slot=\'header\']');
      const footer: HTMLElement = this.el.querySelector('[slot=\'footer\']');

      const gif: HTMLDeckgoGifElement = this.el.shadowRoot.querySelector('deckgo-gif');

      if (gif) {
        if (header) {
          gif.appendChild(header);
        }

        if (footer) {
          gif.appendChild(footer);
        }
      }

      resolve();
    });
  }

  render() {
    return <div class="deckgo-slide">
      <slot name="title"></slot>
      <div class="deckgo-gif-container">
        <slot name="header"></slot>
        <deckgo-gif src={this.src} alt={this.alt} fullscreen={this.fullscreen}></deckgo-gif>
        <slot name="footer"></slot>
      </div>
    </div>
  }

  hostData() {
    return {
      class: {
        'deckgo-slide-container': true,
        'deckgo-slide-container-fullscreen': this.fullscreen
      }
    }
  }

}
