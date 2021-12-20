import {Component, h, ComponentInterface, Prop, Watch} from '@stencil/core';

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
      <svg xmlns="http://www.w3.org/2000/svg" x="0" y="0" height={this.height} width={this.width}>
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

        <foreignObject ref={(el) => (this.foreignObjectRef = el as SVGForeignObjectElement)}>
          {this.text && <p part="text">{this.text}</p>}
        </foreignObject>

        {this.imgSrc && <image x={imgTop} y={imgLeft} width={imgSize} height={imgSize} href={this.imgSrc} part="img" />}
      </svg>
    );
  }
}
