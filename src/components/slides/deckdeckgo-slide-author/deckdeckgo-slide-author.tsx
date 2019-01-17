import {Component, Element, Event, EventEmitter, Method, Prop} from '@stencil/core';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';

@Component({
  tag: 'deckgo-slide-author',
  styleUrl: 'deckdeckgo-slide-author.scss',
  shadow: true
})
export class DeckdeckgoSlideAuthor implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() imgSrc: string;
  @Prop() imgAlt: string;

  async componentDidLoad() {
    await DeckdeckgoUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();
  }

  @Method()
  beforeSwipe(_enter: boolean): Promise<boolean> {
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

  render() {
    return <div class="deckgo-slide">
      <slot name="title"></slot>
      <div class="deckgo-slide-author deckgo-slide-author-start">
        <img data-src={this.imgSrc} alt={this.imgAlt}/>
      </div>
      <div class="deckgo-slide-author deckgo-slide-author-end">
        <slot name="author"></slot>
        <div class="deckgo-slide-author-social">
          <slot name="social-link"></slot>
          <slot name="social-link"></slot>
          <slot name="social-link"></slot>
          <slot name="social-link"></slot>
          <slot name="social-link"></slot>
          <slot name="social-link"></slot>
        </div>
      </div>
      <slot name="notes"></slot>
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
