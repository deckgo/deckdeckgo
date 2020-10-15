import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

import paletteStore from '../../../../../stores/palette.store';

import {ColorUtils, InitStyleColor} from '../../../../../utils/editor/color.utils';
import {PaletteUtils} from '../../../../../utils/editor/palette.utils';

@Component({
  tag: 'app-box-shadow',
})
export class AppBoxShadow {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private boxShadowProperties: Map<string, number> = new Map([
    ['hLength', 0],
    ['vLength', 4],
    ['blurRadius', 16],
    ['spreadRadius', 0],
  ]);

  @State()
  private color: string = '0, 0, 0';

  @State()
  private colorOpacity: number = 12;

  @State()
  private boxShadow: boolean = false;

  @Event() boxShadowDidChange: EventEmitter<void>;

  private readonly DEFAULT_BOX_SHADOW: string = '0 4px 16px rgba(0, 0, 0, 0.12)';

  private readonly MAX_HORIZONTAL_LENGTH: number = 200;
  private readonly MAX_VERTICAL_LENGTH: number = 200;
  private readonly MAX_SPEED_RADIUS: number = 200;
  private readonly MAX_BLUR_RADIUS: number = 100;

  async componentWillLoad() {
    await this.init();
  }

  private async init() {
    if (!this.selectedElement) {
      return;
    }

    const style: CSSStyleDeclaration = window.getComputedStyle(this.selectedElement);

    if (!style) {
      return;
    }

    if (!style.boxShadow || style.boxShadow === 'none') {
      return;
    }

    if (style.boxShadow === 'none') {
      return;
    }

    this.boxShadow = true;

    const rgba: string[] | null = style.boxShadow.match(/rgb.*\)/g);
    const properties: string[] | null = style.boxShadow.split(/rgb.*\)/g);

    if (rgba && rgba.length > 0) {
      const styleColor: InitStyleColor = await ColorUtils.splitColor(rgba[0]);

      this.color = styleColor.rgb ? styleColor.rgb : '0, 0, 0';
      this.colorOpacity = styleColor.rgb ? styleColor.opacity : 12;
    }

    if (properties && properties.length > 0) {
      const notEmptyProperties: string[] = properties.filter((property: string) => property !== '');

      if (notEmptyProperties && notEmptyProperties.length > 0) {
        const boxShadowOtherProperties = notEmptyProperties[0].split(/(\s+)/).filter((e) => e.trim().length > 0);

        this.boxShadowProperties.set('hLength', Number(boxShadowOtherProperties[0].replace('px', '')));
        this.boxShadowProperties.set('vLength', Number(boxShadowOtherProperties[1].replace('px', '')));
        this.boxShadowProperties.set('blurRadius', Number(boxShadowOtherProperties[2].replace('px', '')));
        this.boxShadowProperties.set('spreadRadius', Number(boxShadowOtherProperties[3].replace('px', '')));
        this.boxShadowProperties = new Map<string, number>(this.boxShadowProperties);
      }
    }
  }

  private async selectColor($event: CustomEvent) {
    if (!this.selectedElement || !this.color) {
      return;
    }

    if (!$event || !$event.detail) {
      return;
    }

    $event.stopPropagation();

    await PaletteUtils.updatePalette($event.detail);

    this.color = $event.detail.rgb;

    await this.updateBoxShadow();
  }

  private emitBoxShadowChange() {
    this.boxShadowDidChange.emit();
  }

  private async updateBoxShadowProperties($event: CustomEvent, property: string = '') {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }

    this.boxShadowProperties.set(property, $event.detail.value);
    this.boxShadowProperties = new Map<string, number>(this.boxShadowProperties);

    await this.updateBoxShadow();
  }

  private async updateBoxShadow() {
    const newColor: string = `rgba(${this.color}, ${ColorUtils.transformOpacity(this.colorOpacity)})`;
    this.selectedElement.style.boxShadow = `${this.boxShadowProperties.get('hLength')}px ${this.boxShadowProperties.get(
      'vLength'
    )}px ${this.boxShadowProperties.get('blurRadius')}px ${this.boxShadowProperties.get('spreadRadius')}px ${newColor}`;

    this.emitBoxShadowChange();
  }

  private async toggleBoxShadow() {
    this.boxShadow = !this.boxShadow;

    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.boxShadow = this.boxShadow ? this.DEFAULT_BOX_SHADOW : '';

    if (this.boxShadow) {
      await this.init();
    }

    this.emitBoxShadowChange();
  }

  private async updateOpacity($event: CustomEvent<RangeChangeEventDetail>) {
    if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
      return;
    }

    $event.stopPropagation();

    this.colorOpacity = $event.detail.value as number;

    await this.updateBoxShadow();
  }

  render() {
    return (
      <app-expansion-panel expanded={'close'}>
        <ion-label slot="title">Box shadow</ion-label>
        <ion-list>
          <ion-item>
            <ion-label>{this.boxShadow ? 'Displayed' : 'None'}</ion-label>
            <ion-toggle slot="end" checked={this.boxShadow} mode="md" color="primary" onIonChange={() => this.toggleBoxShadow()}></ion-toggle>
          </ion-item>

          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Horizontal length <small>{this.boxShadowProperties.get('hLength')}px</small>
            </ion-label>
          </ion-item-divider>
          <ion-item>
            <ion-range
              color="primary"
              min={-this.MAX_HORIZONTAL_LENGTH}
              max={this.MAX_HORIZONTAL_LENGTH}
              value={this.boxShadowProperties.get('hLength')}
              mode="md"
              disabled={!this.boxShadow}
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBoxShadowProperties($event, 'hLength')}></ion-range>
          </ion-item>
          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Vertical length <small>{this.boxShadowProperties.get('vLength')}px</small>
            </ion-label>
          </ion-item-divider>
          <ion-item>
            <ion-range
              color="primary"
              min={-this.MAX_VERTICAL_LENGTH}
              max={this.MAX_VERTICAL_LENGTH}
              value={this.boxShadowProperties.get('vLength')}
              mode="md"
              disabled={!this.boxShadow}
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBoxShadowProperties($event, 'vLength')}></ion-range>
          </ion-item>
          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Blur radius <small>{this.boxShadowProperties.get('blurRadius')}px</small>
            </ion-label>
          </ion-item-divider>
          <ion-item>
            <ion-range
              color="primary"
              min={0}
              max={this.MAX_BLUR_RADIUS}
              value={this.boxShadowProperties.get('blurRadius')}
              mode="md"
              disabled={!this.boxShadow}
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBoxShadowProperties($event, 'blurRadius')}></ion-range>
          </ion-item>
          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Spread radius <small>{this.boxShadowProperties.get('spreadRadius')}px</small>
            </ion-label>
          </ion-item-divider>
          <ion-item>
            <ion-range
              color="primary"
              min={-this.MAX_SPEED_RADIUS}
              max={this.MAX_SPEED_RADIUS}
              value={this.boxShadowProperties.get('spreadRadius')}
              mode="md"
              disabled={!this.boxShadow}
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBoxShadowProperties($event, 'spreadRadius')}></ion-range>
          </ion-item>
          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Opacity <small>{this.colorOpacity}</small>
            </ion-label>
          </ion-item-divider>
          <ion-item>
            <ion-range
              color="primary"
              min={0}
              max={100}
              value={this.colorOpacity}
              mode="md"
              disabled={!this.boxShadow}
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateOpacity($event)}></ion-range>
          </ion-item>

          <deckgo-color
            palette={paletteStore.state.palette}
            class="ion-padding"
            onColorChange={($event: CustomEvent) => this.selectColor($event)}
            color-rgb={this.color}>
            <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
          </deckgo-color>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
