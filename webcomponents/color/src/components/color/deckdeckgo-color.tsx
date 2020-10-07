import {Component, h, Prop, EventEmitter, Event, Element, Host, State, Watch} from '@stencil/core';

import {debounce, hexToRgb} from '@deckdeckgo/utils';

import {DeckdeckgoPalette, DeckdeckgoPaletteColor, DEFAULT_PALETTE} from '../../utils/deckdeckgo-palette';

@Component({
  tag: 'deckgo-color',
  styleUrl: 'deckdeckgo-color.scss',
  shadow: true,
})
export class DeckdeckgoColor {
  @Element() el: HTMLElement;

  @Prop({mutable: true}) palette: DeckdeckgoPalette[] = DEFAULT_PALETTE;

  @Prop() more: boolean = true;
  @Prop() moreAlt: string = 'More';

  @Prop() colorHex: string;
  @Prop() colorRgb: string;

  @Prop() label: boolean = true;

  @State()
  private selectedColorHex: string;

  @State()
  private selectedColorRgb: string;

  @State()
  private selectedColorPalette: boolean = false;

  @State()
  private selectedCustomColorRgb: string;

  @State()
  private selectedColorLabel: string;

  @Event()
  colorChange: EventEmitter<DeckdeckgoPaletteColor>;

  private readonly debounceInitSelectedColorPalette: Function;

  constructor() {
    this.debounceInitSelectedColorPalette = debounce(async () => {
      this.selectedColorPalette = await this.initSelectedColorPalette();
      await this.initSelectedColorPaletteAlt();

      this.selectedCustomColorRgb = !this.selectedColorPalette ? this.selectedColorRgb : undefined;
    }, 150);
  }

  async componentWillLoad() {
    this.selectedColorHex = this.colorHex;
    this.selectedColorRgb = this.colorRgb ? this.colorRgb : await hexToRgb(this.colorHex);

    this.selectedColorPalette = await this.initSelectedColorPalette();
    await this.initSelectedColorPaletteAlt();

    if (!this.selectedColorPalette) {
      this.selectedCustomColorRgb = this.selectedColorRgb;
    }
  }

  async componentDidLoad() {
    await this.colorPickerListener(true);
  }

  async disconnectedCallback() {
    await this.colorPickerListener(false);
  }

  @Watch('colorHex')
  async onColorHexChange() {
    this.applyColorHexChange(this.colorHex, undefined);
  }

  private applyColorHexChange(colorHex: string, colorRgb: string | undefined) {
    this.selectedColorHex = colorHex;
    this.selectedColorRgb = colorRgb;

    this.debounceInitSelectedColorPalette();

    // Render component again
    this.palette = [...this.palette];
  }

  @Watch('colorRgb')
  async onColorRgbChange() {
    this.selectedColorHex = undefined;
    this.selectedColorRgb = this.colorRgb;

    this.debounceInitSelectedColorPalette();

    // Render component again
    this.palette = [...this.palette];
  }

  private pickColor(paletteColor: DeckdeckgoPalette): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.palette || this.palette.length <= 0) {
        resolve();
        return;
      }

      this.selectedColorHex = paletteColor.color ? paletteColor.color.hex : undefined;
      this.selectedColorRgb = paletteColor.color ? paletteColor.color.rgb : undefined;

      this.colorChange.emit(paletteColor.color);

      this.selectedColorPalette = true;

      await this.initSelectedColorPaletteAlt();

      this.selectedCustomColorRgb = undefined;

      resolve();
    });
  }

  private openColorPicker(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const colorPicker: HTMLInputElement = this.el.shadowRoot.querySelector("input[name='color-picker']");

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
      const colorPicker: HTMLInputElement = this.el.shadowRoot.querySelector("input[name='color-picker']");

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

    const rgb: string = await hexToRgb(selectedColor);

    const color: DeckdeckgoPaletteColor = {
      hex: selectedColor,
      rgb: rgb,
    };

    this.applyColorHexChange(selectedColor, rgb);

    this.colorChange.emit(color);
  };

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

  private async initSelectedColorPaletteAlt(): Promise<void> {
    if (!this.palette || this.palette.length <= 0) {
      this.selectedColorLabel = undefined;
      return undefined;
    }

    const palette: DeckdeckgoPalette = this.palette.find((element: DeckdeckgoPalette) => {
      return this.isHexColorSelected(element) || this.isRgbColorSelected(element);
    });

    this.selectedColorLabel = palette ? palette.alt : undefined;
  }

  render() {
    return (
      <Host>
        <div class="color-container" part="container">
          {this.renderPalette()}
          {this.renderMore()}
        </div>
        {this.renderLabel()}
      </Host>
    );
  }

  private renderPalette() {
    if (this.palette && this.palette.length > 0) {
      return this.palette.map((element: DeckdeckgoPalette) => {
        const style = {
          '--deckdeckgo-palette-color-hex': `${element.color.hex}`,
          '--deckdeckgo-palette-color-rgb': `${element.color.rgb}`,
        };

        if (element.display) {
          style['--deckdeckgo-palette-border-color'] = element.display.borderColor;
          style['--deckdeckgo-palette-box-shadow-color'] = element.display.boxShadowColor;
        }

        return (
          <button
            aria-label={element.alt}
            class={this.isHexColorSelected(element) || this.isRgbColorSelected(element) ? 'selected' : undefined}
            style={style}
            onClick={() => this.pickColor(element)}></button>
        );
      });
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

      if (!this.selectedColorPalette && this.selectedCustomColorRgb) {
        style['--deckdeckgo-palette-color-rgb'] = this.selectedCustomColorRgb;
      }

      return (
        <div class="more">
          <button
            aria-label={this.more}
            style={style}
            class={!this.selectedColorPalette && (this.selectedColorHex || this.selectedCustomColorRgb) ? 'selected' : undefined}
            onClick={() => this.openColorPicker()}>
            <slot name="more"></slot>
          </button>
          <input type="color" name="color-picker"></input>
        </div>
      );
    } else {
      return undefined;
    }
  }

  private renderLabel() {
    if (!this.label) {
      return undefined;
    }

    return (
      <deckgo-color-label
        colorHex={this.selectedColorHex}
        colorRgb={this.selectedColorRgb}
        colorLabel={this.selectedColorLabel}
        customColorRgb={this.selectedCustomColorRgb}>
        <slot name="custom-label">Custom</slot>
      </deckgo-color-label>
    );
  }
}
