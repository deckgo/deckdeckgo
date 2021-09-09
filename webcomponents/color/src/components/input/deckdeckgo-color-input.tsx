import {Component, EventEmitter, h, Prop, State, Watch, Event, Host} from '@stencil/core';

import {debounce, hexToRgb, rgbToHex} from '@deckdeckgo/utils';

import {DeckdeckgoPaletteColor} from '../../utils/deckdeckgo-palette';

interface InputTargetEvent extends EventTarget {
  value: string;
}

@Component({
  tag: 'deckgo-color-input',
  styleUrl: 'deckdeckgo-color-input.scss',
  shadow: true,
})
export class DeckdeckgoColorInput {
  @Prop()
  colorHex: string;

  @Prop()
  colorRgb: string;

  @Prop()
  customColorRgb: string;

  @Prop()
  inputAlt: string;

  @State()
  private color: string;

  @Event()
  selectHexColor: EventEmitter<DeckdeckgoPaletteColor>;

  private readonly debounceSelectColor: Function;

  constructor() {
    this.debounceSelectColor = debounce(async (inputColor: string) => {
      await this.emitColor(inputColor);
    }, 500);
  }

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

  private async emitColor(inputColor: string) {
    const hex: string = `#${inputColor.replace('#', '')}`;

    const rgb: string | undefined = await hexToRgb(hex);

    if (!rgb) {
      return;
    }

    this.selectHexColor.emit({
      hex,
      rgb,
    });
  }

  render() {
    return (
      <Host>
        <span>#</span>
        <input
          type="text"
          name="color-picker"
          aria-label={this.inputAlt}
          onInput={($event: UIEvent) => this.debounceSelectColor(($event.target as InputTargetEvent).value)}
          value={this.color?.replace('#', '')}
        />
      </Host>
    );
  }
}
