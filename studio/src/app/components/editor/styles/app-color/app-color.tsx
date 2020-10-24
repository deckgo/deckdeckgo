import {Component, EventEmitter, Fragment, h, Prop, State, Event, Watch} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

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
  private color: string;

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

    this.color = rgb;
    this.opacity = opacity ? opacity : 100;
  }

  private async selectColor($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    $event.stopPropagation();

    await PaletteUtils.updatePalette($event.detail);

    this.color = $event.detail.rgb;

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
    this.colorDidChange.emit(`rgba(${this.color},${ColorUtils.transformOpacity(this.opacity)})`);
  }

  private emitReset($event: UIEvent) {
    $event.stopPropagation();

    this.color = null;
    this.opacity = 100;

    this.resetColor.emit();
  }

  private switchInputColor($event: UIEvent, switchColor: 'hex' | 'rgb') {
    $event.stopPropagation();

    paletteStore.state.colorInput = switchColor;
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
      return <ion-input required={true} input-mode="text" name="color" placeholder="#000000" arial-label="Color"></ion-input>;
    } else {
      return (
        <div class="input-rgb">
          <ion-input required={true} input-mode="text" name="color" placeholder="R" arial-label="Rgb - Red"></ion-input>
          <ion-input required={true} input-mode="text" name="color" placeholder="G" arial-label="Rgb - Green"></ion-input>
          <ion-input required={true} input-mode="text" name="color" placeholder="B" arial-label="Rgb - Blue"></ion-input>
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
            disabled={!this.color || this.color === undefined}
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
        onColorChange={($event: CustomEvent) => this.selectColor($event)}
        color-rgb={this.color}>
        <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
      </deckgo-color>
    );
  }
}
