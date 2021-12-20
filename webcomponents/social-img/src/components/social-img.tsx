import {Component, h, ComponentInterface, Prop, Watch} from '@stencil/core';

/**
 * @part text - The CSS pseudo-element to target the paragraph rendered as a child of the SVG foreign object
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
   */
  @Prop()
  padding: number = 64;

  /**
   * A padding to create space around the text and the content
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
          fill="#3dc2ff"
          rx={0}
          ry={0}
          stroke="#3dc2ff"
          stroke-width="5"
        />

        <rect
          x={this.padding}
          y={this.padding}
          width={rectWidth}
          height={rectHeight}
          fill="white"
          rx={0}
          ry={0}
          stroke="#3dc2ff"
          stroke-width="5"
        />

        <foreignObject ref={(el) => (this.foreignObjectRef = el as SVGForeignObjectElement)}>
          {this.text && <p part="text">{this.text}</p>}
        </foreignObject>

        {this.imgSrc && <image x={imgTop} y={imgLeft} width={imgSize} height={imgSize} href={this.imgSrc} />}
      </svg>
    );
  }
}
