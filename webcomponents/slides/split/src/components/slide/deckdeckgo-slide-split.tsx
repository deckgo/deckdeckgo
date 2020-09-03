import {Component, Element, Event, EventEmitter, Method, Prop, h, Host, Build} from '@stencil/core';

import {
  DeckdeckgoSlide,
  hideLazyLoadImages,
  afterSwipe,
  beforeSwipe,
  lazyLoadContent,
  hideAllRevealElements,
  showAllRevealElements,
} from '@deckdeckgo/slide-utils';

@Component({
  tag: 'deckgo-slide-split',
  styleUrl: 'deckdeckgo-slide-split.scss',
  shadow: true,
})
export class DeckdeckgoSlideSplit implements DeckdeckgoSlide {
  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({reflect: true}) vertical: boolean = false;

  @Prop({reflect: true}) type: 'demo' | 'default' = 'default';

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
    return new Promise<void>(async (resolve) => {
      const promises = [];
      promises.push(lazyLoadContent(this.el));
      promises.push(this.resizeDemo());

      await Promise.all(promises);

      resolve();
    });
  }

  @Method()
  revealContent(): Promise<void> {
    return showAllRevealElements(this.el);
  }

  @Method()
  hideContent(): Promise<void> {
    return hideAllRevealElements(this.el);
  }

  private async resizeDemo() {
    if (this.type !== 'demo') {
      return;
    }

    const element: HTMLElement = this.el.querySelector(Build.isBrowser ? ':scope > deckgo-demo' : '> deckgo-demo');

    if (element && typeof (element as any).updateIFrame === 'function') {
      await (element as any).updateIFrame();
    }
  }

  render() {
    const verticalAttr = this.vertical ? '-vertical' : '';
    return (
      <Host class={{'deckgo-slide-container': true}}>
        <div class={`deckgo-slide${verticalAttr}`}>
          <slot name="title"></slot>
          <div class={`deckgo-slide-split${verticalAttr} deckgo-slide-split-start`}>
            <slot name="start"></slot>
          </div>
          <div class={`deckgo-slide-split${verticalAttr} deckgo-slide-split-end`}>
            <slot name="end"></slot>
          </div>
          <slot name="notes"></slot>
          <slot name="actions"></slot>
          <slot name="background"></slot>
          <slot name="header"></slot>
          <slot name="footer"></slot>
        </div>
      </Host>
    );
  }
}
