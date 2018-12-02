import {Component, Element, Event, EventEmitter, Method, Prop} from '@stencil/core';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';

@Component({
  tag: 'deckgo-slide-title',
  styleUrl: 'deckdeckgo-slide-title.scss',
  shadow: true
})
export class DeckdeckgoSlideTitle implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() reveal: boolean = false;
  @Prop() revealShowFirst: boolean = false;

  async componentDidLoad() {
    await DeckdeckgoUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();

    if (this.reveal) {
      await DeckdeckgoSlideUtils.hideRevealElements(this.el, this.revealShowFirst);
    }
  }

  @Method()
  beforeSwipe(_swipeLeft: boolean): Promise<boolean> {
    return DeckdeckgoSlideUtils.beforeSwipe(this.el, _swipeLeft, this.reveal);
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return DeckdeckgoSlideUtils.lazyLoadContent(this.el);
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
