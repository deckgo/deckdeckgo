import {Component, h, Prop, State, Watch} from '@stencil/core';

import {rgbToHex} from '@deckdeckgo/utils';

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
      return rgbToHex(this.customColorRgb);
    }

    return rgbToHex(this.colorRgb);
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
