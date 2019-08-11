import {Component, Element, Event, EventEmitter, Method, Prop, h, Host} from '@stencil/core';

import {DeckDeckGoUtils} from '@deckdeckgo/utils';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoDeckUtils} from '../../utils/deckdeckgo-deck-utils';

@Component({
  tag: 'deckgo-slide-qrcode',
  styleUrl: 'deckdeckgo-slide-qrcode.scss',
  shadow: true
})
export class DeckdeckgoSlideQrcode implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() content: string;

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  async componentDidLoad() {
    await DeckdeckgoDeckUtils.hideLazyLoadImages(this.el);

    this.initWindowResize();

    this.slideDidLoad.emit();
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', DeckDeckGoUtils.debounce(this.onResizeContent));
    }
  }

  private onResizeContent = async () => {
    await this.initQRCodeSize();

    const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-qrcode');

    if (element) {
      await (element as any).generate();
    }
  };

  private initQRCodeSize(): Promise<void> {
    return new Promise<void>((resolve) => {
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-qrcode');

      if (container) {
        const width: number = container.clientWidth;
        const height: number = container.clientHeight;

        const qrCode: HTMLElement = container.querySelector('deckgo-qrcode');

        if (qrCode) {
          qrCode.style.setProperty('--deckgo-qrcode-size', width > height ? (height + 'px') : ('calc('  + width + 'px - 32px)'));
        }
      }

      resolve();
    });
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
    return new Promise<void>(async (resolve) => {
      const promises = [];
      promises.push(DeckdeckgoSlideUtils.lazyLoadContent(this.el));
      promises.push(this.initQRCodeSize());

      await Promise.all(promises);

      resolve();
    });
  }

  @Method()
  revealContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  hideContent(): Promise<void> {
    return Promise.resolve();
  }

  render() {
    return <Host class={{'deckgo-slide-container': true}}>
      <div class="deckgo-slide">
        <slot name="title"></slot>
        <div class="deckgo-slide-qrcode">
          <slot name="content"></slot>
          <deckgo-qrcode content={this.content}></deckgo-qrcode>
        </div>
        <slot name="notes"></slot>
        <slot name="actions"></slot>
        <slot name="background"></slot>
      </div>
    </Host>;
  }

}
