import {Component, Element, Prop, Listen} from '@stencil/core';

import {DeckDeckGoUtils} from '../../utils/deckdeckgo-utils';

@Component({
  tag: 'deckgo-gif',
  styleUrl: 'deckdeckgo-gif.scss',
  shadow: true
})
export class DeckdeckgoGif {

  @Element() el: HTMLElement;

  @Prop() gif: string;
  @Prop() alt: string;

  @Prop() fullscreen: boolean = false;

  async componentDidLoad() {
    this.lazyLoadGif();
  }

  @Listen('gif')
  async lazyLoadGif() {
    await DeckDeckGoUtils.lazyLoadImages(this.el);
  }

  render() {
    return <div class="deckgo-gif">
      <slot name="header"></slot>
      <img data-src={this.gif} alt={this.alt}/>
      <slot name="footer"></slot>
    </div>;
  }

  hostData() {
    return {
      class: {
        'deckgo-gif-fullscreen': this.fullscreen
      }
    }
  }
}
