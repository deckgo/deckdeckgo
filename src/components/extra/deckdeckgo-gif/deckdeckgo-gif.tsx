import {Component, Element, Prop, Method, State} from '@stencil/core';

import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';
import {DeckdeckgoExtra} from '../deckdeckgo-extra';

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

  async componentDidLoad() {
    await DeckdeckgoUtils.hideLazyLoadImages(this.el);

    const img: HTMLImageElement = this.el.shadowRoot.querySelector('img');

    if (img) {
      img.addEventListener('load', () => {
        this.loaded = true;
      }, false);
    }
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return DeckdeckgoUtils.lazyLoadImages(this.el);
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
