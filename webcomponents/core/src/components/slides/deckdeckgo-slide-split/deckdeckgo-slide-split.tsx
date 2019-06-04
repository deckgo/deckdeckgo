import {Component, Element, Event, EventEmitter, Method, Prop, h} from '@stencil/core';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoDeckUtils} from '../../utils/deckdeckgo-deck-utils';

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

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  async componentDidLoad() {
    await DeckdeckgoDeckUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();

    if (this.reveal) {
      await DeckdeckgoSlideUtils.hideRevealElements(this.el, this.revealShowFirst);
    }
  }

  @Method()
  beforeSwipe(enter: boolean): Promise<boolean> {
    return DeckdeckgoSlideUtils.beforeSwipe(this.el, enter, this.reveal);
  }

  @Method()
  afterSwipe(): Promise<void> {
    return DeckdeckgoSlideUtils.afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return DeckdeckgoSlideUtils.lazyLoadContent(this.el);
  }

  render() {
    return <div class="deckgo-slide">
      <slot name="title"></slot>
      <div class="deckgo-slide-split deckgo-slide-split-start"><slot name="start"></slot></div>
      <div class="deckgo-slide-split deckgo-slide-split-end"><slot name="end"></slot></div>
      <slot name="notes"></slot>
      <slot name="actions"></slot>
      <slot name="background"></slot>
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
