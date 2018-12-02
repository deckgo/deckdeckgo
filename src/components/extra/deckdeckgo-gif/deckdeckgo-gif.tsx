import {Component, Element, Prop, Method} from '@stencil/core';

import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';

@Component({
  tag: 'deckgo-gif',
  styleUrl: 'deckdeckgo-gif.scss',
  shadow: true
})
export class DeckdeckgoGif {

  @Element() el: HTMLElement;

  @Prop() src: string;
  @Prop() alt: string;

  @Prop() fullscreen: boolean = false;

  async componentDidLoad() {
    await DeckdeckgoUtils.hideLazyLoadImages(this.el);
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
        'deckgo-gif-fullscreen': this.fullscreen
      }
    }
  }
}
