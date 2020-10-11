import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

import paletteStore from '../../../../../stores/palette.store';

import {ColorUtils, InitStyleColor} from '../../../../../utils/editor/color.utils';
import {PaletteUtils} from '../../../../../utils/editor/palette.utils';

@Component({
  tag: 'app-color-word-cloud',
})
export class AppColorWordCloud {
  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  moreColors: boolean = true;

  @State()
  private color: string;

  @State()
  private colorOpacity: number = 100;

  @Event() wordCloudDidChange: EventEmitter<void>;

  @State()
  private colorIndex: number = 1;

  private indexes: number[] = [...Array(99).keys()];

  async componentWillLoad() {
    await this.initColor();
  }

  private selectColor($event: CustomEvent, colorFunction: Function): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || !this.selectedElement.parentElement) {
        resolve();
        return;
      }

      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      await PaletteUtils.updatePalette($event.detail);

      colorFunction($event);

      this.emitChange();

      resolve();
    });
  }

  private setColor = async ($event: CustomEvent) => {
    this.color = $event.detail.rgb ?? $event.detail.hex;
    await this.applyColor();
  };

  private applyColor(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.color) {
        resolve();
        return;
      }

      const selectedColor: string = `rgba(${this.color},${ColorUtils.transformOpacity(this.colorOpacity)})`;

      this.selectedElement.style.setProperty(this.getStyle(), selectedColor);

      resolve();
    });
  }

  private getStyle(): string {
    return `--deckgo-word-count-fill-color-${this.colorIndex}`;
  }

  // prettier-ignore
  private initColor(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || !this.selectedElement.style) {
        this.color = undefined;
        this.colorOpacity = 100;

        resolve();
        return;
      }

      const styleColor: InitStyleColor = await ColorUtils.splitColor(this.selectedElement.style.getPropertyValue(this.getStyle()));

      this.color = styleColor.rgb;
      this.colorOpacity = styleColor.opacity;

      resolve();
    });
  }

  private emitChange() {
    this.wordCloudDidChange.emit();
  }

  private updateOpacity($event: CustomEvent<RangeChangeEventDetail>, opacityFunction: Function): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
        resolve();
        return;
      }

      $event.stopPropagation();

      const opacity: number = $event.detail.value as number;

      opacityFunction(opacity);

      this.emitChange();

      resolve();
    });
  }

  private setOpacity = async (opacity: number) => {
    this.colorOpacity = opacity;
    await this.applyColor();
  };

  private resetColor(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      this.selectedElement.style.removeProperty(this.getStyle());

      await this.initColor();

      this.emitChange();

      resolve();
    });
  }

  private async selectColorIndex($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    const input: string = $event.detail.value;

    if (!isNaN(input as any)) {
      this.colorIndex = parseInt(input);

      await this.initColor();
    }
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Colors</ion-label>
        <ion-list>
          <ion-item class="select">
            <ion-label>Series</ion-label>

            <ion-select
              value={this.colorIndex}
              placeholder="Series index"
              onIonChange={(e: CustomEvent) => this.selectColorIndex(e)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              {this.renderChartIndexes()}
            </ion-select>
          </ion-item>

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
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateOpacity($event, this.setOpacity)}></ion-range>
          </ion-item>

          <div class="ion-padding-start">
            <deckgo-color
              palette={paletteStore.state.palette}
              class="ion-padding-bottom"
              onColorChange={($event: CustomEvent) => this.selectColor($event, this.setColor)}
              color-rgb={this.color}
              more={this.moreColors}>
              <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
            </deckgo-color>
          </div>

          <ion-item class="action-button ion-margin-bottom">
            <ion-button shape="round" onClick={() => this.resetColor()} fill="outline" class="delete">
              <ion-label>Reset color</ion-label>
            </ion-button>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }

  // A select is more user friendly than an input
  private renderChartIndexes() {
    return this.indexes.map((index: number) => {
      return <ion-select-option value={index + 1}>{`Word ${index + 1}`}</ion-select-option>;
    });
  }
}
