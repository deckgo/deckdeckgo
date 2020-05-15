import {Component, h, Prop, State, Watch} from '@stencil/core';

@Component({
  tag: 'deckgo-color-label',
  styleUrl: 'deckdeckgo-color-label.scss',
  shadow: true,
})
export class DeckdeckgoColorLabel {
  @Prop()
  colorHex: string;

  @Prop()
  colorRgb: string;

  @Prop()
  colorLabel: string;

  @Prop()
  customColorRgb: string;

  @State()
  private color: string;

  async componentWillLoad() {
    this.color = await this.initColorHex();
  }

  @Watch('colorHex')
  @Watch('colorRgb')
  @Watch('customColorRgb')
  async watchColors() {
    this.color = await this.initColorHex();
  }

  async initColorHex(): Promise<string> {
    if (this.colorHex) {
      return this.colorHex;
    }

    if (this.customColorRgb) {
      return this.rgbToHex(this.customColorRgb);
    }

    return this.rgbToHex(this.colorRgb);
  }

  // https://stackoverflow.com/a/42429333/5404186
  async rgbToHex(rgb: string | undefined): Promise<string | undefined> {
    if (!rgb) {
      return undefined;
    }

    const toHex = (rgb): string => `#${rgb.map((v) => v.toString(16).padStart(2, '0')).join('')}`;
    const extractRgb = (rgb): number[] =>
      rgb
        .match(/(\d+),\s*(\d+),\s*(\d+)/)
        .splice(1, 3)
        .map((v) => Number(v));

    return toHex(extractRgb(rgb));
  }

  render() {
    if (!this.colorHex && !this.customColorRgb && !this.colorRgb) {
      return <p class="color-label"></p>;
    }

    return (
      <p class="color-label">
        <span>{this.colorLabel ? this.colorLabel : <slot></slot>}</span> <small>{this.color}</small>
      </p>
    );
  }
}
