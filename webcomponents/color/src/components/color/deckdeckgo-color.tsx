import {Component, h, Prop, EventEmitter, Event, Element} from '@stencil/core';

import {DeckdeckgoPalette, DEFAULT_PALETTE} from '../utils/deckdeckgo-palette';

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

  @Prop({mutable: true}) highlighted: string;

  @Event()
  selected: EventEmitter<string>;

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

      this.highlighted = paletteColor.color.hex;

      this.selected.emit(paletteColor.color.hex);

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
    const color: string = $event.target.value;

    this.highlighted = undefined;

    this.selected.emit(color);
  };

  render() {
    return <div class="color-container">
      {this.renderPalette()}
      {this.renderMore()}
    </div>;
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
                         class={this.highlighted && this.highlighted === element.color.hex ? 'selected' : undefined}
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
