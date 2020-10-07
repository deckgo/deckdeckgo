import {Component, Element, Event, EventEmitter, h, Method, Prop, State, Watch} from '@stencil/core';
import {RangeChangeEventDetail} from '@ionic/core';

import paletteStore from '../../../../stores/palette.store';

import {ColorUtils, InitStyleColor} from '../../../../utils/editor/color.utils';
import {PaletteUtils} from '../../../../utils/editor/palette.utils';

@Component({
  tag: 'app-color-text-background',
})
export class AppColorTextBackground {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  moreColors: boolean = true;

  @Prop()
  slide: boolean = false;

  @Prop()
  deck: boolean = false;

  @Prop()
  colorType: 'text' | 'background' = 'text';

  @Prop()
  expanded: boolean = true;

  @State()
  private color: string;

  @State()
  private colorOpacity: number = 100;

  @Event() colorChange: EventEmitter<void>;

  async componentWillLoad() {
    await this.initCurrentColors();
  }

  @Watch('colorType')
  async onColorTypeChange() {
    await this.initCurrentColors();
  }

  @Method()
  async initCurrentColors() {
    if (!this.selectedElement) {
      return;
    }

    let styleColor: InitStyleColor;

    // prettier-ignore
    if (this.colorType === 'background') {
      styleColor = await ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--background') ? this.selectedElement.style.getPropertyValue('--background') : this.selectedElement.style.background);
    } else {
      styleColor = await ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--color') ? this.selectedElement.style.getPropertyValue('--color') : this.selectedElement.style.color);
    }

    this.color = styleColor.rgb;
    this.colorOpacity = styleColor.opacity;
  }

  private async selectColor($event: CustomEvent) {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }

    await PaletteUtils.updatePalette($event.detail);

    this.color = $event.detail.rgb;

    await this.applyColor();
  }

  private async applyColor() {
    if (this.colorType === 'background') {
      await this.applyBackground();
    } else {
      await this.applyTextColor();
    }
  }

  private resetColor(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (this.colorType === 'background') {
        this.selectedElement.style.removeProperty('--background');
        this.selectedElement.style.removeProperty('background');
      } else {
        this.selectedElement.style.removeProperty('--color');
        this.selectedElement.style.removeProperty('color');
      }

      this.color = null;
      this.colorOpacity = 100;

      this.colorChange.emit();

      resolve();
    });
  }

  private applyTextColor(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.color) {
        resolve();
        return;
      }

      const selectedColor: string = `rgba(${this.color},${ColorUtils.transformOpacity(this.colorOpacity)})`;

      if (this.deck || this.slide) {
        this.selectedElement.style.setProperty('--color', selectedColor);
      } else {
        this.selectedElement.style.color = selectedColor;
      }

      this.colorChange.emit();

      resolve();
    });
  }

  private applyBackground(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.color) {
        resolve();
        return;
      }

      const selectedColor: string = `rgba(${this.color},${ColorUtils.transformOpacity(this.colorOpacity)})`;

      if (this.deck || this.slide) {
        this.selectedElement.style.setProperty('--background', selectedColor);
      } else {
        this.selectedElement.style.background = selectedColor;
      }

      this.colorChange.emit();

      resolve();
    });
  }

  private updateOpacity($event: CustomEvent<RangeChangeEventDetail>): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
        resolve();
        return;
      }

      $event.stopPropagation();

      const opacity: number = $event.detail.value as number;

      this.colorOpacity = opacity;

      await this.applyColor();

      resolve();
    });
  }

  render() {
    return (
      <app-expansion-panel expanded={this.expanded ? 'open' : 'close'}>
        <ion-label slot="title">Color</ion-label>
        <ion-list class="ion-no-padding">
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
        </ion-list>
        <deckgo-color
          palette={paletteStore.state.palette}
          class="ion-padding-start ion-padding-end ion-padding-bottom"
          more={this.moreColors}
          onColorChange={($event: CustomEvent) => this.selectColor($event)}
          color-rgb={this.color}>
          <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
        </deckgo-color>
        <ion-item class="action-button ion-margin-bottom">
          <ion-button shape="round" onClick={() => this.resetColor()} fill="outline" class="delete" disabled={this.color === null}>
            <ion-label>Reset color</ion-label>
          </ion-button>
        </ion-item>
      </app-expansion-panel>
    );
  }
}
