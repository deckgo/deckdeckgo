import {Component, EventEmitter, Fragment, h, Prop, Event} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

import paletteStore from '../../../../stores/palette.store';

import {PaletteUtils} from '../../../../utils/editor/palette.utils';

@Component({
  tag: 'app-color',
  styleUrl: 'app-color.scss',
})
export class AppImage {
  @Prop()
  color: string;

  @Prop()
  colorOpacity: number = 100;

  @Event()
  colorDidChange: EventEmitter<{color: string; opacity: number}>;

  @Event()
  resetColor: EventEmitter<void>;

  private async selectColor($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    $event.stopPropagation();

    await PaletteUtils.updatePalette($event.detail);

    this.color = $event.detail.rgb;

    this.emitChange();
  }

  private async updateOpacity($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
      return;
    }

    $event.stopPropagation();

    const opacity: number = $event.detail.value as number;

    this.colorOpacity = opacity;

    this.emitChange();
  }

  private emitChange() {
    this.colorDidChange.emit({
      color: this.color,
      opacity: this.colorOpacity,
    });
  }

  private emitReset($event: UIEvent) {
    $event.stopPropagation();

    this.resetColor.emit();
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
      <ion-item class="with-padding">
        <ion-fab-button size="small" slot="start" arial-label="Color picker"></ion-fab-button>
        <ion-input required={true} input-mode="text" name="color" placeholder="#000000" arial-label="Color"></ion-input>
        <button slot="end" arial-label="Reset" onClick={($event: UIEvent) => this.emitReset($event)}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </button>
      </ion-item>
    );
  }

  private renderOpacity() {
    return (
      <Fragment>
        <ion-item-divider class="ion-padding-top">
          <ion-label>
            Opacity <small>{this.colorOpacity}%</small>
          </ion-label>
        </ion-item-divider>
        <ion-item class="item-opacity">
          <ion-range
            color="primary"
            min={0}
            max={100}
            disabled={!this.color || this.color === undefined}
            value={this.colorOpacity}
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
