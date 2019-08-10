import {Component, Element, Event, EventEmitter, Method, Prop, h, Host} from '@stencil/core';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoDeckUtils} from '../../utils/deckdeckgo-deck-utils';

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

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  async componentDidLoad() {
    await DeckdeckgoDeckUtils.hideLazyLoadImages(this.el);

    this.slideDidLoad.emit();
  }

  @Method()
  beforeSwipe(_enter: boolean, _reveal: boolean): Promise<boolean> {
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
    return <Host class={{'deckgo-slide-container': true}}>
        <div class="deckgo-slide">
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
        <slot name="actions"></slot>
        <slot name="background"></slot>
      </div>
    </Host>;
  }

}
