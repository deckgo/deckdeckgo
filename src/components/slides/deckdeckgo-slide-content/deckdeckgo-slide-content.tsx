import {Component, Element, Event, EventEmitter, Method, Prop} from '@stencil/core';

import {DeckdeckgoSlide, DeckDeckGoSlideUtils} from '../deckdeckgo-slide';
import {DeckDeckGoUtils} from '../../utils/deckdeckgo-utils';

@Component({
  tag: 'deckgo-slide-content',
  styleUrl: 'deckdeckgo-slide-content.scss',
  shadow: true
})
export class DeckdeckgoSlideContent implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() reveal: boolean = false;
  @Prop() revealShowFirst: boolean = false;

  async componentDidLoad() {
    await DeckDeckGoSlideUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();

    if (this.reveal) {
      await DeckDeckGoSlideUtils.hideRevealElements(this.el, this.revealShowFirst);
    }
  }

  @Method()
  beforeSwipe(_swipeLeft: boolean): Promise<boolean> {
    return DeckDeckGoSlideUtils.beforeSwipe(this.el, _swipeLeft, this.reveal);
  }

  @Method()
  lazyLoadImages(): Promise<void> {
    return DeckDeckGoUtils.lazyLoadImages(this.el);
  }

  render() {
    return <div class="deckgo-slide">
      <slot name="title"></slot>
      <slot name="content"></slot>
    </div>;
  }

  hostData() {
    return {
      class: {
        'deckgo-slide-container': true
      }
    }
  }

}
