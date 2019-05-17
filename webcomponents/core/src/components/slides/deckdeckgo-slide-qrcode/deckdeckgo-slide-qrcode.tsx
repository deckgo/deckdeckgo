import {Component, Element, Event, EventEmitter, Method, Prop} from '@stencil/core';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';

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
    await DeckdeckgoUtils.hideLazyLoadImages(this.el);

    this.initWindowResize();

    this.slideDidLoad.emit();
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', DeckdeckgoUtils.debounce(this.onResizeContent));
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
    return new Promise<void>(async (resolve) => {
      const promises = [];
      promises.push(DeckdeckgoSlideUtils.lazyLoadContent(this.el));
      promises.push(this.initQRCodeSize());

      await Promise.all(promises);

      resolve();
    });
  }

  render() {
    return <div class="deckgo-slide">
      <slot name="title"></slot>
      <div class="deckgo-slide-qrcode">
        <slot name="content"></slot>
        <deckgo-qrcode content={this.content}></deckgo-qrcode>
      </div>
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
