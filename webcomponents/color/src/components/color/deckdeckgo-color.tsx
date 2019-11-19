import {Component, h, Prop, EventEmitter, Event, Element, Host, State, Watch} from '@stencil/core';

import {DeckdeckgoPalette, DeckdeckgoPaletteColor, DEFAULT_PALETTE} from '../utils/deckdeckgo-palette';

@Component({
  tag: 'deckgo-color',
  styleUrl: 'deckdeckgo-color.scss',
  shadow: true
})
export class DeckdeckgoColor {

  @Element() el: HTMLElement;

  @Prop({mutable: true}) palette: DeckdeckgoPalette[] = DEFAULT_PALETTE;

  @Prop() more: boolean = true;
  @Prop() moreAlt: string = 'More';

  @Prop() colorHex: string;
  @Prop() colorRgb: string;

  @State()
  private selectedColorHex: string;

  @State()
  private selectedColorRgb: string;

  @State()
  private selectedColorPalette: boolean = false;

  @Event()
  colorChange: EventEmitter<DeckdeckgoPaletteColor>;

  async componentWillLoad() {
    this.selectedColorHex = this.colorHex;
    this.selectedColorRgb = this.colorRgb ? this.colorRgb : await this.hexToRgb(this.colorHex);

    this.selectedColorPalette = await this.initSelectedColorPalette();
  }

  async componentDidLoad() {
    await this.colorPickerListener(true);
  }

  async componentDidUnload() {
    await this.colorPickerListener(false);
  }

  @Watch('colorHex')
  async onColorHexChange() {
    // To avoid quick repaint glitch
    this.selectedColorPalette = true;

    this.selectedColorHex = this.colorHex;
    this.selectedColorRgb = await this.hexToRgb(this.colorHex);

    this.selectedColorPalette = await this.initSelectedColorPalette();

    // Render component again
    this.palette = [...this.palette];
  }

  @Watch('colorRgb')
  async onColorRgbChange() {
    // To avoid quick repaint glitch
    this.selectedColorPalette = true;

    this.selectedColorHex = undefined;
    this.selectedColorRgb = this.colorRgb;

    this.selectedColorPalette = await this.initSelectedColorPalette();

    // Render component again
    this.palette = [...this.palette];
  }

  private pickColor(paletteColor: DeckdeckgoPalette): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.palette || this.palette.length <= 0) {
        resolve();
        return;
      }

      this.selectedColorHex = paletteColor.color ? paletteColor.color.hex : undefined;
      this.selectedColorRgb = paletteColor.color ? paletteColor.color.rgb : undefined;

      this.colorChange.emit(paletteColor.color);

      this.selectedColorPalette = true;

      resolve();
    });
  }

  private openColorPicker(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const colorPicker: HTMLInputElement = this.el.shadowRoot.querySelector('input[name=\'color-picker\']');

      if (!colorPicker) {
        resolve();
        return;
      }

      colorPicker.click();

      resolve();
    });
  }

  private colorPickerListener(bind: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      const colorPicker: HTMLInputElement = this.el.shadowRoot.querySelector('input[name=\'color-picker\']');

      if (!colorPicker) {
        resolve();
        return;
      }

      if (bind) {
        colorPicker.addEventListener('change', this.selectColor, false);
      } else {
        colorPicker.removeEventListener('change', this.selectColor, true);
      }

      resolve();
    });
  }

  private selectColor = async ($event) => {
    const selectedColor: string = $event.target.value;

    const color: DeckdeckgoPaletteColor = {
      hex: selectedColor,
      rgb: await this.hexToRgb(selectedColor)
    };

    this.colorHex = selectedColor;

    this.colorChange.emit(color);
  };

  // https://stackoverflow.com/a/5624139/5404186
  private hexToRgb(hex: string): Promise<string> {
    return new Promise<string>((resolve) => {
      if (!hex || hex === undefined || hex === '') {
        resolve(undefined);
        return;
      }

      const result: RegExpExecArray | null = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);

      resolve(result ? `${parseInt(result[1], 16)}, ${parseInt(result[2], 16)}, ${parseInt(result[3], 16)}` : undefined);
    });
  }

  private isHexColorSelected(element: DeckdeckgoPalette): boolean {
    if (!element || !element.color || !element.color.hex) {
      return false;
    }

    if (!this.selectedColorHex) {
      return false;
    }

    return this.selectedColorHex.toUpperCase() === element.color.hex.toUpperCase();
  }

  private isRgbColorSelected(element: DeckdeckgoPalette): boolean {
    if (!element || !element.color || !element.color.rgb) {
      return false;
    }

    if (!this.selectedColorRgb) {
      return false;
    }

    return this.selectedColorRgb.replace(/\s/g, '').toUpperCase() === element.color.rgb.replace(/\s/g, '').toUpperCase();
  }

  private initSelectedColorPalette(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!this.palette || this.palette.length <= 0) {
        resolve(false);
        return;
      }

      const index: number = this.palette.findIndex((element: DeckdeckgoPalette) => {
        return this.isHexColorSelected(element) || this.isRgbColorSelected(element);
      });

      resolve(index > -1);
    });
  }

  render() {
    return <Host>
      <div class="color-container">
        {this.renderPalette()}
        {this.renderMore()}
      </div>
    </Host>;
  }

  private renderPalette() {
    if (this.palette && this.palette.length > 0) {
      return (
        this.palette.map((element: DeckdeckgoPalette) => {

          const style = {
            '--deckdeckgo-palette-color-hex': `${element.color.hex}`,
            '--deckdeckgo-palette-color-rgb': `${element.color.rgb}`
          };

          return <button aria-label={element.alt}
                         class={this.isHexColorSelected(element) || this.isRgbColorSelected(element) ? 'selected' : undefined}
                         style={style} onClick={() => this.pickColor(element)}>
          </button>
        })
      );
    } else {
      return undefined;
    }
  }

  private renderMore() {
    if (this.more) {
      let style = {};

      if (!this.selectedColorPalette && this.selectedColorHex) {
        style['--deckdeckgo-palette-color-hex'] = this.selectedColorHex;
      }

      if (!this.selectedColorPalette && this.selectedColorRgb) {
        style['--deckdeckgo-palette-color-rgb'] = this.selectedColorRgb;
      }

      return <div class="more">
        <button aria-label={this.more} style={style}
                class={!this.selectedColorPalette && (this.selectedColorHex || this.selectedColorRgb) ? 'selected' : undefined}
                onClick={() => this.openColorPicker()}>
          <slot name="more"></slot>
        </button>
        <input type="color" name="color-picker"></input>
      </div>
    } else {
      return undefined;
    }
  }
}
