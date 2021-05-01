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

  /**
   * The palette of color.
   */
  @Prop({mutable: true})
  palette: DeckdeckgoPalette[] = DEFAULT_PALETTE;

  /**
   * An accessibility label for the color input field
   */
  @Prop() inputAlt: string = 'Input a color (hex)';

  /**
   * The current selected color provided as hexadecimal value
   */
  @Prop() colorHex: string;
  /**
   * The current selected color provided as a rgb value
   */
  @Prop() colorRgb: string;

  @State()
  private selectedColorHex: string;

  @State()
  private selectedColorRgb: string;

  @State()
  private selectedColorPalette: boolean = false;

  @State()
  private selectedCustomColorRgb: string;

  /**
   * Emit the selected color
   */
  @Event()
  colorChange: EventEmitter<DeckdeckgoPaletteColor>;

  private readonly debounceInitSelectedColorPalette: Function;

  constructor() {
    this.debounceInitSelectedColorPalette = debounce(async () => {
      this.selectedColorPalette = await this.initSelectedColorPalette();

      this.selectedCustomColorRgb = !this.selectedColorPalette ? this.selectedColorRgb : undefined;
    }, 150);
  }

  async componentWillLoad() {
    this.selectedColorHex = this.colorHex;
    this.selectedColorRgb = this.colorRgb ? this.colorRgb : await hexToRgb(this.colorHex);

    this.selectedColorPalette = await this.initSelectedColorPalette();

    if (!this.selectedColorPalette) {
      this.selectedCustomColorRgb = this.selectedColorRgb;
    }
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

      this.selectedCustomColorRgb = undefined;

      resolve();
    });
  }

  private selectColor = async ($event: CustomEvent<DeckdeckgoPaletteColor>) => {
    const color = $event.detail;

    this.applyColorHexChange(color.hex, color.rgb);

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

  render() {
    return (
      <Host>
        {this.renderPalette()}
        {this.renderInput()}
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

  private renderInput() {
    return (
      <deckgo-color-input
        colorHex={this.selectedColorHex}
        colorRgb={this.selectedColorRgb}
        customColorRgb={this.selectedCustomColorRgb}
        inputAlt={this.inputAlt}
        onSelectHexColor={this.selectColor}></deckgo-color-input>
    );
  }
}
