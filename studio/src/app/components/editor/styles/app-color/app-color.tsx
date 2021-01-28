import {Component, EventEmitter, Fragment, h, Prop, State, Event, Watch, Method} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

import {DeckdeckgoPalette, DeckdeckgoPaletteColor} from '@deckdeckgo/color';

import {debounce, extractRgb, hexToRgb, rgbToHex} from '@deckdeckgo/utils';

import colorStore from '../../../../stores/color.store';

import {ColorUtils, InitStyleColor} from '../../../../utils/editor/color.utils';
import settingsStore from '../../../../stores/settings.store';
import {EditMode} from '../../../../types/core/settings';

@Component({
  tag: 'app-color',
  styleUrl: 'app-color.scss',
})
export class AppColor {
  @Prop()
  initColor: () => Promise<InitStyleColor>;

  @Prop()
  defaultColor: string;

  @State()
  private color: {hex: string; rgb: {value: string; r: number; g: number; b: number}};

  @State()
  private opacity: number = 100;

  @State()
  private colorCSS: string;

  @Event()
  colorDidChange: EventEmitter<string>;

  @Event()
  resetColor: EventEmitter<void>;

  private destroyListener;

  private readonly debounceHandleHexInput: ($event: CustomEvent<KeyboardEvent>) => void = debounce(async ($event: CustomEvent<KeyboardEvent>) => {
    await this.handleHexInput($event);
  }, 500);

  private readonly debounceHandleRgbInput: ($event: CustomEvent<KeyboardEvent>, colorType: 'r' | 'g' | 'b') => void = debounce(
    async ($event: CustomEvent<KeyboardEvent>, colorType: 'r' | 'g' | 'b') => {
      await this.handleRgbInput($event, colorType);
    },
    500
  );

  async componentWillLoad() {
    await this.loadColor();

    this.destroyListener = settingsStore.onChange('edit', async (_edit: EditMode) => {
      await this.loadColor();
    });
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  @Watch('initColor')
  async onInitColorChange() {
    await this.loadColor();
  }

  @Method()
  async loadColor() {
    const {rgb, opacity} = await this.initColor();

    await this.initColorStateRgb(rgb);

    this.opacity = opacity ? opacity : 100;

    await this.initColorCSS();
  }

  private async initColorCSS() {
    if (!this.color.rgb?.value) {
      return;
    }

    this.colorCSS = `rgba(${this.color.rgb.value}, ${this.opacity / 100})`;
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

  private async selectColor($event: UIEvent, color: DeckdeckgoPaletteColor) {
    if (!$event || !color) {
      return;
    }

    $event.stopPropagation();

    await this.initColorStateRgb(color.rgb);
    await this.initColorCSS();

    await this.colorChange();
  }

  private async updateOpacity($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
      return;
    }

    $event.stopPropagation();

    const opacity: number = $event.detail.value as number;

    this.opacity = opacity;

    await this.colorChange();
  }

  private async colorChange() {
    this.colorDidChange.emit(`rgba(${this.color.rgb.value},${ColorUtils.transformOpacity(this.opacity)})`);

    await ColorUtils.updateColor({
      hex: this.color.hex,
      rgb: this.color.rgb.value,
    });
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

    colorStore.state.colorInput = switchColor;
  }

  private async handleHexInput($event: CustomEvent<KeyboardEvent>) {
    const input: string = ($event.target as InputTargetEvent).value;

    if (!/^#?([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})/.test(input)) {
      return;
    }

    await this.initColorStateHex(input);

    await this.colorChange();
  }

  private async handleRgbInput($event: CustomEvent<KeyboardEvent>, colorType: 'r' | 'g' | 'b') {
    const input: number = parseInt(($event.target as InputTargetEvent).value);

    if (isNaN(input) || input < 0 || input > 255) {
      return;
    }

    this.color.rgb[colorType] = input;

    if (this.color.rgb.r >= 0 && this.color.rgb.g >= 0 && this.color.rgb.b >= 0) {
      await this.initColorStateRgb(`${this.color.rgb.r}, ${this.color.rgb.g}, ${this.color.rgb.b}`);

      await this.colorChange();
    }
  }

  private async onColorPickerChange($event) {
    await this.initColorStateHex($event.target.value);

    await this.colorChange();
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.colorCSS = ($event.target as InputTargetEvent).value;
  }

  private async updateColorCSS() {
    this.colorDidChange.emit(this.colorCSS);
  }

  render() {
    return (
      <Fragment>
        <ion-list class="inputs-list standard">
          {this.renderColorPicker()}
          {this.renderOpacity()}
        </ion-list>

        <ion-list class="css">
          <ion-item class="with-padding ion-margin-bottom">
            <ion-input
              value={this.colorCSS}
              placeholder="Color code"
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
              onIonChange={async () => await this.updateColorCSS()}></ion-input>
          </ion-item>
        </ion-list>

        {this.renderColorHistory()}
      </Fragment>
    );
  }

  private renderColorPicker() {
    const colorValue: string = !this.color || !this.color.hex ? this.defaultColor : this.color.hex.charAt(0) === '#' ? this.color.hex : `#${this.color.hex}`;

    return (
      <div class="color-picker item-input">
        <input type="color" slot="start" arial-label="Color picker" value={colorValue} onChange={($event) => this.onColorPickerChange($event)}></input>
        {this.renderColorInput()}
        <button slot="end" class="reset" arial-label="Reset" onClick={($event: UIEvent) => this.emitReset($event)}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </button>

        {this.renderColorSwitcher()}
      </div>
    );
  }

  private renderColorInput() {
    if (colorStore.state.colorInput === 'hex') {
      return (
        <ion-input
          debounce={500}
          input-mode="tel"
          max-length={7}
          onIonInput={($event: CustomEvent<KeyboardEvent>) => this.debounceHandleHexInput($event)}
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
            onIonInput={($event: CustomEvent<KeyboardEvent>) => this.debounceHandleRgbInput($event, 'r')}
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
            onIonInput={($event: CustomEvent<KeyboardEvent>) => this.debounceHandleRgbInput($event, 'g')}
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
            onIonInput={($event: CustomEvent<KeyboardEvent>) => this.debounceHandleRgbInput($event, 'b')}
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
      <div class={`switcher ${colorStore.state.colorInput}`}>
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
            min={0}
            max={100}
            disabled={!this.color || this.color === undefined || !this.color.hex}
            value={this.opacity}
            mode="md"
            style={{'--bar-background-active': this.color.hex}}
            onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateOpacity(e)}></ion-range>
        </ion-item>
      </Fragment>
    );
  }

  private renderColorHistory() {
    return <div class="history ion-padding-start ion-padding-end ion-padding-bottom">{this.renderPalette()}</div>;
  }

  private renderPalette() {
    return colorStore.state.history.map((palette: DeckdeckgoPalette) => {
      return (
        <ion-fab-button
          size="small"
          style={{'--background': palette.color.hex}}
          onClick={($event: UIEvent) => this.selectColor($event, palette.color)}></ion-fab-button>
      );
    });
  }
}
