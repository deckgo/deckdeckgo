import {Component, Element, Event, EventEmitter, Method, Prop, h, Host} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';
import {DeckdeckgoSlideResize, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

/**
 * @slot title - A title
 * @slot content - An optional content
 * @slot notes - Some notes related to this slide
 * @slot actions - Custom actions for this slide
 * @slot background - A custom background for this slide
 * @slot header - A custom header for this slide
 * @slot footer - A custom footer for this slide
 */
@Component({
  tag: 'deckgo-slide-qrcode',
  styleUrl: 'deckdeckgo-slide-qrcode.scss',
  shadow: true,
})
export class DeckdeckgoSlideQrcode implements DeckdeckgoSlideResize {
  @Element() el: HTMLElement;

  /**
   * Triggered when the slide is loaded
   */
  @Event() slideDidLoad: EventEmitter<void>;

  /**
   * The content, a text or an url, of the QR code to generate
   */
  @Prop({reflect: true}) content: string;

  /**
   * In case you would like to display a logo over the QR code, provide the source of the image. Note: this image is lazy loaded too
   */
  @Prop({reflect: true}) imgSrc: string;
  /**
   * In case you would display a logo over the QR code, you could provide an accessibility attribute using this option
   */
  @Prop({reflect: true}) imgAlt: string;

  /**
   * If you define a background for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customBackground: boolean = false;

  /**
   * If you provide actions for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customActions: boolean = false;

  private container!: HTMLDivElement;
  private qrCode!: HTMLElement;

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.initWindowResize();

    this.slideDidLoad.emit();
  }

  async componentDidUpdate() {
    const img: HTMLImageElement = this.el.shadowRoot.querySelector('img');

    if (img && this.imgSrc) {
      await this.lazyLoadContent();
    }
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onResizeContent));
    }
  }

  private onResizeContent = async () => {
    await this.initQRCodeSize();

    const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-qrcode');

    if (element) {
      await (element as any).generate();
    }
  };

  private async initQRCodeSize() {
    if (!this.container || !this.qrCode) {
      return;
    }

    const title: HTMLElement = this.el.querySelector(':scope > [slot="title"]');

    if (!title) {
      return;
    }

    const style: CSSStyleDeclaration = window.getComputedStyle(this.container);

    if (!style) {
      return;
    }

    const qrCodeSafeAreaSize: number = 16;
    const spacing: number = title.clientHeight + qrCodeSafeAreaSize;

    const size: number =
      (parseFloat(style.width) < parseFloat(style.height)
        ? parseFloat(style.width) - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight)
        : parseFloat(style.height) - parseFloat(style.paddingTop) - parseFloat(style.paddingBottom)) - spacing;

    this.qrCode.style.setProperty('--deckgo-qrcode-size', size + 'px');
  }

  @Method()
  beforeSwipe(_enter: boolean, _reveal: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true);
    });
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

  @Method()
  resizeContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.onResizeContent();

      resolve();
    });
  }

  render() {
    return (
      <Host class={{'deckgo-slide-container': true}}>
        <div class="deckgo-slide" ref={(el) => (this.container = el as HTMLDivElement)}>
          <slot name="title"></slot>
          <div class="deckgo-slide-qrcode">
            <slot name="content"></slot>
            <deckgo-qrcode content={this.content} ref={(el) => (this.qrCode = el as HTMLElement)}>
              {this.renderLogo()}
            </deckgo-qrcode>
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

  private renderLogo() {
    if (this.imgSrc) {
      return <img slot="logo" data-src={this.imgSrc} alt={this.imgAlt} />;
    } else {
      return undefined;
    }
  }
}
