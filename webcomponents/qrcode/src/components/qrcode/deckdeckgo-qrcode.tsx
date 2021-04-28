import {Component, Element, Method, Prop, h, Watch} from '@stencil/core';

import * as QRCodeGenerator from '../utils/qrcode-generator/qrcode';

enum DeckdeckgoQRCodeType {
  SVG = 'svg',
  IMG = 'img'
}

/**
 * @slot logo - An optional logo or image to be displayed over the QR code
 */
@Component({
  tag: 'deckgo-qrcode',
  styleUrl: 'deckdeckgo-qrcode.scss',
  shadow: true
})
export class DeckdeckgoQRCode {
  @Element() el: HTMLElement;

  /**
   * The content, a text or an url, of the QR code to generate
   */
  @Prop() content: string;

  /**
   * The type of QR code to generate, <svg/> or <img/>
   */
  @Prop() type: string = DeckdeckgoQRCodeType.SVG;

  /**
   * The size of the cell, useful to generate a bigger QR code, specially in case of <img/>. Use it wisely, I suggest a value between 0 and 20 for example
   */
  @Prop() qrCellSize: number;
  /**
   * The size of the code margin, in case you would like more spacing
   */
  @Prop() qrMargin: number;
  /**
   * The background color of the QR code. The value should be provided in a RGB-hex format. For example: FF0000
   */
  @Prop() qrBackgroundColor: string;
  /**
   * The color use to fill the QR code. The value should be provided in a RGB-hex format. For example: FF0000
   */
  @Prop() qrFillColor: string;
  /**
   * An alternate text for the image of the QR code
   */
  @Prop() qrAlt: string;

  async componentDidLoad() {
    await this.generate();
  }

  @Watch('content')
  async onContentChange() {
    await this.generate();
  }

  /**
   * The <deckgo-qrcode/> component exposes the following method in case you would like to refresh your QR code, for example on resize of the window on in case you would set its content asynchronously.
   */
  @Method()
  generate(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const qrCode: string = await this.createQRCode();
      await this.parseQRCode(qrCode);

      resolve();
    });
  }

  private createQRCode(): Promise<string> {
    return new Promise<string>((resolve) => {
      if (!this.content || this.content.length <= 0) {
        resolve(null);
        return;
      }

      QRCodeGenerator.qrcode.stringToBytesFuncs['UTF-8'];

      const qrGenerator = QRCodeGenerator.qrcode(0, 'H');
      qrGenerator.addData(this.content, 'Byte');
      qrGenerator.make();

      const qrCode: string = this.isQRCodeTypeImg()
        ? qrGenerator.createImgTag(this.qrCellSize, this.qrMargin, this.qrAlt, this.qrFillColor, this.qrBackgroundColor)
        : qrGenerator.createSvgTag(this.qrCellSize, this.qrMargin);

      resolve(qrCode);
    });
  }

  private parseQRCode(qrCode: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!qrCode) {
        resolve();
        return;
      }

      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-qrcode-container');

      if (container) {
        this.removePreviousQRCode(container);

        try {
          const template = document.createElement('template');
          template.innerHTML = qrCode;

          container.appendChild(template.content.firstChild);
        } catch (err) {
          // Then we don't generate a QR code
        }
      }

      resolve();
    });
  }

  private removePreviousQRCode(container: HTMLElement) {
    const svg: SVGSVGElement = container.querySelector('svg');
    if (svg) {
      svg.parentNode.removeChild(svg);
    }

    const img: HTMLImageElement = container.querySelector('img');
    if (img) {
      img.parentNode.removeChild(img);
    }
  }

  private isQRCodeTypeImg() {
    return this.type === DeckdeckgoQRCodeType.IMG;
  }

  render() {
    return (
      <div class="deckgo-qrcode-container">
        <slot name="logo"></slot>
      </div>
    );
  }
}
