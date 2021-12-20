import {Component, h, ComponentInterface, Prop, Method, State, Host} from '@stencil/core';
import {canvasToBlob, svgToCanvas} from '../utils/svg.utils';
import {fetchImage} from '../utils/image.utils';

/**
 * @part text - The CSS pseudo-element to target the paragraph rendered as a child of the SVG foreign object
 * @part image - The CSS pseudo-element to target the image displayed as a logo
 */
@Component({
  tag: 'deckgo-social-img',
  styleUrl: 'social-img.scss',
  shadow: true
})
export class SocialImg implements ComponentInterface {
  private foreignObjectRef!: SVGForeignObjectElement;
  private svgRef!: SVGGraphicsElement;

  /**
   * The social image width
   * @default 1200px
   */
  @Prop()
  width: string = '1200px';

  /**
   * The social image width
   * @default 1200px
   */
  @Prop()
  height: string = '628px';

  /**
   * A padding to create space around the content
   * @default 64
   */
  @Prop()
  padding: number = 64;

  /**
   * A padding to create space around the text and the content
   * @default 32
   */
  @Prop()
  innerPadding: number = 32;

  /**
   * The text to display (per default clamped with three dots "..." after some lines, see CSS)
   */
  @Prop()
  text: string;

  /**
   * An optional image (https://....) that can for example be displayed as logo.
   * Note: it will be fetched and transformed to base64. The SVG won't be rendered until the logo is loaded.
   */
  @Prop()
  imgSrc: string;

  /**
   * The mime type of the image. Default 'image/svg+xml'
   */
  @Prop()
  imgMimeType: string = 'image/svg+xml';

  /**
   * The width of the stroke of the rectangles
   * @default 4
   */
  @Prop()
  rectStrokeWidth: number = 5;

  /**
   * The color for rectangles
   * @default #3dc2ff
   */
  @Prop()
  rectColor: string = '#3dc2ff';

  @State()
  private imgBase64: {
    state: 'loading' | 'loaded' | 'none';
    value: string | undefined;
  };

  componentWillLoad() {
    this.imgBase64 = {
      state: this.imgSrc && this.imgMimeType ? 'loading' : 'none',
      value: undefined
    };

    this.loadImage().then(
      (value: string | undefined) =>
        (this.imgBase64 = {
          state: 'loaded',
          value
        })
    );
  }

  componentDidLoad() {
    this.setForeignObjectAttributes();
  }

  componentDidUpdate() {
    this.setForeignObjectAttributes();
  }

  /**
   * Transform the rendered svg to an image
   * @param type The type of output (default 'image/webp')
   * @returns The image as blob
   */
  @Method()
  async toBlob(type: string = 'image/webp'): Promise<Blob> {
    const canvas: HTMLCanvasElement = await svgToCanvas({svg: this.svgRef});
    return canvasToBlob({canvas, type});
  }

  private setForeignObjectAttributes() {
    this.foreignObjectRef?.setAttribute('x', `${this.padding + this.innerPadding}`);
    this.foreignObjectRef?.setAttribute('y', `${this.padding + this.innerPadding}`);
    this.foreignObjectRef?.setAttribute('width', `${parseInt(this.width) - 2 * this.padding - (this.imgSrc ? 5 : 2) * this.innerPadding}`);
    this.foreignObjectRef?.setAttribute('height', `${parseInt(this.height) - 2 * this.padding - 2 * this.innerPadding}`);
  }

  private async loadImage(): Promise<string | undefined> {
    if (!this.imgSrc || !this.imgMimeType) {
      return undefined;
    }

    try {
      const base64: string | undefined = await fetchImage({imgSrc: this.imgSrc});
      return base64;
    } catch (err) {
      console.log('Cannot fetch and transform image, ignored.');
      console.error(err);
      return undefined;
    }
  }

  render() {
    return <Host>{this.renderSVG()}</Host>;
  }

  private renderSVG() {
    const rectWidth: number = parseInt(this.width) - 2 * this.padding;
    const rectHeight: number = parseInt(this.height) - 2 * this.padding;

    const imgTop: number = parseInt(this.width) - 2 * this.padding - this.innerPadding;
    const imgLeft: number = parseInt(this.height) - 2 * this.padding - this.innerPadding;
    const imgSize: number = this.innerPadding * 2;

    // We use a custom state as we have to force a re-rendering of the svg when loaded and because we also want to display something if the svg cannot be loaded.
    // Also because rendering an image tag within the svg asynchronously is ignored by the browser.
    if (!this.imgBase64 || this.imgBase64.state === 'loading') {
      return undefined;
    }

    return (
      <svg
        xmlns="http://www.w3.org/2000/svg"
        x="0"
        y="0"
        height={this.height}
        width={this.width}
        ref={(el) => (this.svgRef = el as SVGGraphicsElement)}>
        <rect
          x={this.padding + 16}
          y={this.padding + 16}
          width={rectWidth}
          height={rectHeight}
          fill={this.rectColor}
          rx={0}
          ry={0}
          stroke={this.rectColor}
          stroke-width={this.rectStrokeWidth}
        />

        <rect
          x={this.padding}
          y={this.padding}
          width={rectWidth}
          height={rectHeight}
          fill="white"
          rx={0}
          ry={0}
          stroke={this.rectColor}
          stroke-width={this.rectStrokeWidth}
        />

        {this.text && (
          <foreignObject xmlns="http://www.w3.org/1999/xhtml" ref={(el) => (this.foreignObjectRef = el as SVGForeignObjectElement)}>
            <p part="text">{this.text}</p>
          </foreignObject>
        )}

        {this.imgBase64?.value && (
          <image
            x={imgTop}
            y={imgLeft}
            width={imgSize}
            height={imgSize}
            href={`data:${this.imgMimeType};base64,${this.imgBase64.value}`}
            part="img"
          />
        )}
      </svg>
    );
  }
}
