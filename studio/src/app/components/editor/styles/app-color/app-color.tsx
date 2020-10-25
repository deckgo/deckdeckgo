import {Component, EventEmitter, Fragment, h, Prop, State, Event, Watch} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

import {extractRgb, hexToRgb, rgbToHex} from '@deckdeckgo/utils';

import paletteStore from '../../../../stores/palette.store';

import {PaletteUtils} from '../../../../utils/editor/palette.utils';
import {ColorUtils, InitStyleColor} from '../../../../utils/editor/color.utils';

@Component({
  tag: 'app-color',
  styleUrl: 'app-color.scss',
})
export class AppImage {
  @Prop()
  initColor: () => Promise<InitStyleColor>;

  @State()
  private color: {hex: string; rgb: {value: string; r: number; g: number; b: number}};

  @State()
  private opacity: number = 100;

  @Event()
  colorDidChange: EventEmitter<string>;

  @Event()
  resetColor: EventEmitter<void>;

  async componentWillLoad() {
    await this.loadColor();
  }

  @Watch('initColor')
  async loadColor() {
    const {rgb, opacity} = await this.initColor();

    await this.initColorStateRgb(rgb);

    this.opacity = opacity ? opacity : 100;
  }

  private async initColorStateRgb(rgb: string | undefined) {
    const splitRgb: number[] | undefined = rgb ? extractRgb(rgb) : undefined;

    this.color = {
      hex: await rgbToHex(rgb),
      rgb: {
        value: rgb,
        r: splitRgb?.[0],
        g: splitRgb?.[1],
        b: splitRgb?.[2],
      },
    };
  }

  private async initColorStateHex(hex: string | undefined) {
    const rgb: string | undefined = await hexToRgb(hex);

    const splitRgb: number[] | undefined = rgb ? extractRgb(rgb) : undefined;

    this.color = {
      hex: hex,
      rgb: {
        value: rgb,
        r: splitRgb?.[0],
        g: splitRgb?.[1],
        b: splitRgb?.[2],
      },
    };
  }

  private async selectColor($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    $event.stopPropagation();

    await PaletteUtils.updatePalette($event.detail);

    await this.initColorStateRgb($event.detail.rgb);

    this.emitRgbaColorChange();
  }

  private async updateOpacity($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
      return;
    }

    $event.stopPropagation();

    const opacity: number = $event.detail.value as number;

    this.opacity = opacity;

    this.emitRgbaColorChange();
  }

  private emitRgbaColorChange() {
    this.colorDidChange.emit(`rgba(${this.color.rgb.value},${ColorUtils.transformOpacity(this.opacity)})`);
  }

  private emitReset($event: UIEvent) {
    $event.stopPropagation();

    this.color = {
      hex: undefined,
      rgb: {
        value: undefined,
        r: undefined,
        g: undefined,
        b: undefined,
      },
    };
    this.opacity = 100;

    this.resetColor.emit();
  }

  private switchInputColor($event: UIEvent, switchColor: 'hex' | 'rgb') {
    $event.stopPropagation();

    paletteStore.state.colorInput = switchColor;
  }

  private async handleHexInput($event: CustomEvent<KeyboardEvent>) {
    const input: string = ($event.target as InputTargetEvent).value;

    if (!/^#?([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})/.test(input)) {
      return;
    }

    await this.initColorStateHex(input);

    this.emitRgbaColorChange();
  }

  private async handleRgbInput($event: CustomEvent<KeyboardEvent>, colorType: 'r' | 'g' | 'b') {
    const input: number = parseInt(($event.target as InputTargetEvent).value);

    if (isNaN(input) || input < 0 || input > 255) {
      return;
    }

    this.color.rgb[colorType] = input;

    if (this.color.rgb.r >= 0 && this.color.rgb.g >= 0 && this.color.rgb.b >= 0) {
      await this.initColorStateRgb(`${this.color.rgb.r}, ${this.color.rgb.g}, ${this.color.rgb.b}`);

      this.emitRgbaColorChange();
    }
  }

  render() {
    return (
      <Fragment>
        <ion-list class="inputs-list">
          {this.renderColorPicker()}
          {this.renderOpacity()}
        </ion-list>

        {this.renderColorHistory()}
      </Fragment>
    );
  }

  private renderColorPicker() {
    return (
      <div class="color-picker item-input">
        <ion-fab-button size="small" slot="start" arial-label="Color picker"></ion-fab-button>
        {this.renderColorInput()}
        <button slot="end" class="reset" arial-label="Reset" onClick={($event: UIEvent) => this.emitReset($event)}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </button>

        {this.renderColorSwitcher()}
      </div>
    );
  }

  private renderColorInput() {
    if (paletteStore.state.colorInput === 'hex') {
      return (
        <ion-input
          debounce={500}
          input-mode="tel"
          max-length={7}
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleHexInput($event)}
          required={true}
          value={this.color?.hex}
          name="color"
          placeholder="#000000"
          arial-label="Color"></ion-input>
      );
    } else {
      return (
        <div class="input-rgb">
          <ion-input
            input-mode="tel"
            value={this.color?.rgb?.r}
            debounce={500}
            max-length={3}
            onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleRgbInput($event, 'r')}
            min={'0'}
            max={'255'}
            name="r"
            placeholder="R"
            arial-label="Rgb - Red"></ion-input>
          <ion-input
            input-mode="tel"
            value={this.color?.rgb?.g}
            debounce={500}
            max-length={3}
            onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleRgbInput($event, 'g')}
            min={'0'}
            max={'255'}
            name="g"
            placeholder="G"
            arial-label="Rgb - Green"></ion-input>
          <ion-input
            input-mode="tel"
            value={this.color?.rgb?.b}
            debounce={500}
            max-length={3}
            onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleRgbInput($event, 'b')}
            min={'0'}
            max={'255'}
            name="b"
            placeholder="B"
            arial-label="Rgb - Blue"></ion-input>
        </div>
      );
    }
  }

  private renderColorSwitcher() {
    return (
      <div class={`switcher ${paletteStore.state.colorInput}`}>
        <button onClick={($event: UIEvent) => this.switchInputColor($event, 'hex')}>hex</button>{' '}
        <button onClick={($event: UIEvent) => this.switchInputColor($event, 'rgb')}>rgb</button>
      </div>
    );
  }

  private renderOpacity() {
    return (
      <Fragment>
        <ion-item-divider class="ion-padding-top">
          <ion-label>
            Opacity <small>{this.opacity}%</small>
          </ion-label>
        </ion-item-divider>
        <ion-item class="item-opacity">
          <ion-range
            color="primary"
            min={0}
            max={100}
            disabled={!this.color || this.color === undefined || !this.color.hex}
            value={this.opacity}
            mode="md"
            onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateOpacity(e)}></ion-range>
        </ion-item>
      </Fragment>
    );
  }

  private renderColorHistory() {
    return (
      <deckgo-color
        palette={paletteStore.state.palette}
        class="ion-padding-start ion-padding-end ion-padding-bottom"
        more={false}
        label={false}
        onColorChange={($event: CustomEvent) => this.selectColor($event)}>
        <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
      </deckgo-color>
    );
  }
}
