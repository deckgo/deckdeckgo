import {Component, Element, Method, Prop, h, Watch} from '@stencil/core';

import * as QRCodeGenerator from '../utils/qrcode-generator/qrcode';

enum DeckdeckgoQRCodeType {
  SVG = 'svg',
  IMG = 'img'
}

@Component({
  tag: 'deckgo-qrcode',
  styleUrl: 'deckdeckgo-qrcode.scss',
  shadow: true
})
export class DeckdeckgoQRCode {
  @Element() el: HTMLElement;

  @Prop() content: string;

  @Prop() type: string = DeckdeckgoQRCodeType.SVG;

  @Prop() qrCellSize: number;
  @Prop() qrMargin: number;

  @Prop() qrBackgroundColor: string;
  @Prop() qrFillColor: string;
  @Prop() qrAlt: string;

  async componentDidLoad() {
    await this.generate();
  }

  @Watch('content')
  async onContentChange() {
    await this.generate();
  }

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
