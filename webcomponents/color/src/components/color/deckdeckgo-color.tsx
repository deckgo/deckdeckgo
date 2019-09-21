import {Component, h, Prop, EventEmitter, Event, Element, Host} from '@stencil/core';

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

  @Prop({mutable: true}) color: string;

  @Event()
  colorChange: EventEmitter<DeckdeckgoPaletteColor>;

  async componentDidLoad() {
    await this.colorPickerListener(true);
  }

  async componentDidUnload() {
    await this.colorPickerListener(false);
  }

  private pickColor(paletteColor: DeckdeckgoPalette): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.palette || this.palette.length <= 0) {
        resolve();
        return;
      }

      this.color = paletteColor.color ? paletteColor.color.hex : undefined;

      this.colorChange.emit(paletteColor.color);

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

    this.color = undefined;

    this.colorChange.emit({
      hex: selectedColor,
      rgb: await this.hexToRgb(selectedColor)
    });
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

    if (!this.color) {
      return false;
    }

    return this.color.toUpperCase() === element.color.hex.toUpperCase();
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

          let style = {
            '--deckdeckgo-palette-color-hex': `${element.color.hex}`,
            '--deckdeckgo-palette-color-rgb': `${element.color.rgb}`
          };

          return <button aria-label={element.alt}
                         class={this.isHexColorSelected(element) ? 'selected' : undefined}
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
      return <div class="more">
        <button aria-label={this.more}
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
