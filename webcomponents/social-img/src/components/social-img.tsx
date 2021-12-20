import {Component, h, ComponentInterface, Prop, Watch, Method} from '@stencil/core';
import {canvasToBlob, svgToCanvas} from '../utils/svg.utils';

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
   */
  @Prop()
  imgSrc: string;

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

  componentDidLoad() {
    this.setForeignObjectAttributes();
  }

  @Watch('width')
  @Watch('height')
  @Watch('padding')
  @Watch('innerPadding')
  @Watch('imgSrc')
  onPropChanges() {
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

  render() {
    const rectWidth: number = parseInt(this.width) - 2 * this.padding;
    const rectHeight: number = parseInt(this.height) - 2 * this.padding;

    const imgTop: number = parseInt(this.width) - 2 * this.padding - this.innerPadding;
    const imgLeft: number = parseInt(this.height) - 2 * this.padding - this.innerPadding;
    const imgSize: number = this.innerPadding * 2;

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
          <foreignObject ref={(el) => (this.foreignObjectRef = el as SVGForeignObjectElement)}>
            <p part="text">{this.text}</p>
          </foreignObject>
        )}

        {this.imgSrc && <image x={imgTop} y={imgLeft} width={imgSize} height={imgSize} href={this.imgSrc} part="img" />}
      </svg>
    );
  }
}
