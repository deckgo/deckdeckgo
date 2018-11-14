import {Component, Element, Event, EventEmitter, Method, Prop} from '@stencil/core';

import {DeckdeckgoSlide, DeckDeckGoSlideUtils} from '../deckdeckgo-slide';

@Component({
  tag: 'deckgo-slide-author',
  styleUrl: 'deckdeckgo-slide-author.scss',
  shadow: true
})
export class DeckdeckgoSlideAuthor implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() imgUrl: string;

  async componentDidLoad() {
    this.slideDidLoad.emit();
  }

  @Method()
  beforeSwipe(_swipeLeft: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true)
    });
  }

  @Method()
  lazyLoadImages(): Promise<void> {
    return DeckDeckGoSlideUtils.lazyLoadImages(this.el);
  }

  render() {
    return <div class="deckgo-slide">
      <div class="deckgo-slide-author deckgo-slide-author-start">
        <img data-src={this.imgUrl}/>
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
