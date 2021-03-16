import {Component, Element, Event, EventEmitter, Method, h, Host, Prop} from '@stencil/core';

import {
  DeckdeckgoSlide,
  hideLazyLoadImages,
  afterSwipe,
  beforeSwipe,
  lazyLoadContent,
  hideAllRevealElements,
  showAllRevealElements
} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-slide-svg',
  styleUrl: 'slide-svg.scss',
  shadow: true
})
export class SlideSvg implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({reflect: true})
  svgSrc: string;

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

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
    return (
      <Host class={{'deckgo-slide-container': true}} custom-background={true}>
        <div class="deckgo-slide">
          <div class="deckgo-aspect-ratio-container">
            <div class="deckgo-aspect-ratio-content">
              <deckgo-lazy-img svgSrc={this.svgSrc}></deckgo-lazy-img>

              <slot />
            </div>
          </div>
        </div>

        <slot name="notes"></slot>
        <slot name="actions"></slot>
        <slot name="header"></slot>
        <slot name="footer"></slot>
      </Host>
    );
  }
}
