import {Component, Element, Event, EventEmitter, Method, Prop} from '@stencil/core';

import {DeckdeckgoSlide, DeckDeckGoSlideUtils} from '../deckdeckgo-slide';

@Component({
  tag: 'deckgo-slide-split',
  styleUrl: 'deckdeckgo-slide-split.scss',
  shadow: true
})
export class DeckdeckgoSlideSplit implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() reveal: boolean = false;
  @Prop() revealShowFirst: boolean = false;

  async componentDidLoad() {
    this.slideDidLoad.emit();

    if (this.reveal) {
      await DeckDeckGoSlideUtils.hideElements(this.el, this.revealShowFirst);
    }
  }

  @Method()
  beforeSwipe(_swipeLeft: boolean): Promise<boolean> {
    return DeckDeckGoSlideUtils.beforeSwipe(this.el, _swipeLeft, this.reveal);
  }

  @Method()
  lazyLoadImages(): Promise<void> {
    return DeckDeckGoSlideUtils.lazyLoadImages(this.el);
  }

  render() {
    return <div class="deckgo-slide">
      <slot name="title"></slot>
      <div class="deckgo-slide-split deckgo-slide-split-start"><slot name="start"></slot></div>
      <div class="deckgo-slide-split deckgo-slide-split-end"><slot name="end"></slot></div>
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
