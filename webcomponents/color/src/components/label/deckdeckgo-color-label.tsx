import {Component, h, Prop} from '@stencil/core';

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

  render() {
    if (!this.colorHex && !this.customColorRgb && !this.colorRgb) {
      return <p class="color-label"></p>;
    }

    const color: string = `${this.colorHex ? this.colorHex : `rgb(${this.customColorRgb ? this.customColorRgb : this.colorRgb})`}`;

    return (
      <p class="color-label">
        <span>{this.colorLabel ? this.colorLabel : <slot></slot>}</span> <small>{color}</small>
      </p>
    );
  }
}
