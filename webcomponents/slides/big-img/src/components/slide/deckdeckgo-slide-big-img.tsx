import {Component, Element, Event, EventEmitter, Method, Prop, h, Host} from '@stencil/core';

import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, beforeSwipe, lazyLoadContent, hideAllRevealElements, showAllRevealElements} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-slide-big-img',
  styleUrl: 'deckdeckgo-slide-big-img.scss',
  shadow: true
})
export class DeckdeckgoSlideBigImg implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;
  @Prop() src: string = '';

  private image: HTMLElement;

  async componentDidLoad() {
    this.image = this.el.shadowRoot.querySelector('.image');
    this.image.style.backgroundImage = `url(${this.src})`;

    this.slideDidLoad.emit();
  }

  @Method()
  beforeSwipe(enter: boolean, reveal: boolean): Promise<boolean> {
    return beforeSwipe(this.el, enter, reveal);
  }

  @Method()
  afterSwipe(): Promise<void> {
    return afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return lazyLoadContent(this.el);
  }

  @Method()
  revealContent(): Promise<void> {
    return showAllRevealElements(this.el);
  }

  @Method()
  hideContent(): Promise<void> {
    return hideAllRevealElements(this.el);
  }

  render() {
    return <Host class={{'deckgo-slide-container': true}}>
    <div class="deckgo-slide">
      <div class="image"></div>
    </div>
  </Host>;
  }

}
