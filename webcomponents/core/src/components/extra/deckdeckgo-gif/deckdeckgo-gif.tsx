import {Component, Element, Prop, Method, State, Event, EventEmitter, h} from '@stencil/core';

import {DeckdeckgoExtra} from '../deckdeckgo-extra';
import {DeckdeckgoDeckUtils} from '../../utils/deckdeckgo-deck-utils';

@Component({
  tag: 'deckgo-gif',
  styleUrl: 'deckdeckgo-gif.scss',
  shadow: true
})
export class DeckdeckgoGif implements DeckdeckgoExtra {

  @Element() el: HTMLElement;

  @Prop() src: string;
  @Prop() alt: string;

  @Prop() fullscreen: boolean = false;

  @State() loaded: boolean = false;

  @Event() gifLoaded: EventEmitter<boolean>;

  async componentDidLoad() {
    await DeckdeckgoDeckUtils.hideLazyLoadImages(this.el);

    const img: HTMLImageElement = this.el.shadowRoot.querySelector('img');

    if (img) {
      img.addEventListener('load', () => {
        this.loaded = true;
        this.gifLoaded.emit(true);
      }, false);
    }
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return DeckdeckgoDeckUtils.lazyLoadImages(this.el);
  }

  render() {
    return <div class="deckgo-gif">
      <slot name="header"></slot>
      <img data-src={this.src} alt={this.alt}/>
      <slot name="footer"></slot>
    </div>;
  }

  hostData() {
    return {
      class: {
        'deckgo-gif-fullscreen': this.fullscreen,
        'deckgo-gif-hidden': !this.loaded
      }
    }
  }
}
