import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {ColorUtils} from '../../../../../utils/editor/color.utils';

import {RangeChangeEventDetail} from '@ionic/core';

@Component({
  tag: 'app-box-shadow',
})
export class AppBoxShadow {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private boxShadowProperties: Map<string, number> = new Map([
    ['hLength', 0],
    ['vLength', 0],
    ['blurRadius', 0],
    ['spreadRadius', 0],
    ['opacity', 100],
  ]);

  @State()
  private color: string = '#000000';

  @State()
  private maxHLength: number = 0;

  @State()
  private maxVLength: number = 0;

  @State()
  private maxBlurRadius: number = 0;

  @Event() boxShadowDidChange: EventEmitter<void>;

  async componentWillLoad() {
    if (this.selectedElement) {
      this.maxHLength = this.selectedElement.offsetWidth / 2;
      this.maxVLength = this.selectedElement.offsetHeight / 2;
      this.maxBlurRadius = this.selectedElement.offsetHeight * 0.75;

      const style: CSSStyleDeclaration = window.getComputedStyle(this.selectedElement);

      if (!style) {
        return;
      }

      if (style.boxShadow === 'none') {
        return;
      } else {
        const boxShadowSplitColor = style.boxShadow.split(')');
        const initialColorRgba = `${boxShadowSplitColor[0]})`;
        this.color = ColorUtils.rgbaToHex(`${boxShadowSplitColor[0]})`);
        this.boxShadowProperties.set(
          'opacity',
          Number(initialColorRgba.substring(initialColorRgba.lastIndexOf(',') + 1, initialColorRgba.lastIndexOf(')')).trim()) * 100
        );
        const boxShadowOtherProperties = boxShadowSplitColor[1].split(/(\s+)/).filter((e) => e.trim().length > 0);
        this.boxShadowProperties.set('hLength', Number(boxShadowOtherProperties[0].replace('px', '')));
        this.boxShadowProperties.set('vLength', Number(boxShadowOtherProperties[1].replace('px', '')));
        this.boxShadowProperties.set('blurRadius', Number(boxShadowOtherProperties[2].replace('px', '')));
        this.boxShadowProperties.set('spreadRadius', Number(boxShadowOtherProperties[3].replace('px', '')));
        this.boxShadowProperties = new Map<string, number>(this.boxShadowProperties);
      }
    }
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

      colorFunction($event);

      resolve();
    });
  }

  private setCodeColor = async ($event: CustomEvent) => {
    this.color = $event.detail.hex;
    await this.applyCodeColor();
  };

  private applyCodeColor(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.color) {
        resolve();
        return;
      }
      const newColor = ColorUtils.hexToRGBA(this.color, ColorUtils.transformOpacity(this.boxShadowProperties.get('opacity')));
      this.selectedElement.style.boxShadow = `${this.boxShadowProperties.get('hLength')}px ${this.boxShadowProperties.get(
        'vLength'
      )}px ${this.boxShadowProperties.get('blurRadius')}px ${this.boxShadowProperties.get('spreadRadius')}px ${newColor}`;
      this.emitBoxShadowChange();
      resolve();
    });
  }

  private emitBoxShadowChange() {
    this.boxShadowDidChange.emit();
  }

  private async updateBoxShadow($event: CustomEvent, property: string = ''): Promise<void> {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }
    this.boxShadowProperties.set(property, $event.detail.value);
    this.boxShadowProperties = new Map<string, number>(this.boxShadowProperties);
    const newColor = ColorUtils.hexToRGBA(this.color, ColorUtils.transformOpacity(this.boxShadowProperties.get('opacity')));
    this.selectedElement.style.boxShadow = `${this.boxShadowProperties.get('hLength')}px ${this.boxShadowProperties.get(
      'vLength'
    )}px ${this.boxShadowProperties.get('blurRadius')}px ${this.boxShadowProperties.get('spreadRadius')}px ${newColor}`;
    this.emitBoxShadowChange();
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Box shadow</ion-label>
        <ion-list>
          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Horizontal length <small>{this.boxShadowProperties.get('hLength')}px</small>
            </ion-label>
          </ion-item-divider>
          <ion-item class="item-opacity">
            <ion-range
              color="primary"
              min={-this.maxHLength}
              max={this.maxHLength}
              value={this.boxShadowProperties.get('hLength')}
              mode="md"
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBoxShadow($event, 'hLength')}></ion-range>
          </ion-item>
          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Vertical length <small>{this.boxShadowProperties.get('vLength')}px</small>
            </ion-label>
          </ion-item-divider>
          <ion-item class="item-opacity">
            <ion-range
              color="primary"
              min={-this.maxVLength}
              max={this.maxVLength}
              value={this.boxShadowProperties.get('vLength')}
              mode="md"
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBoxShadow($event, 'vLength')}></ion-range>
          </ion-item>
          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Blur radius <small>{this.boxShadowProperties.get('blurRadius')}px</small>
            </ion-label>
          </ion-item-divider>
          <ion-item class="item-opacity">
            <ion-range
              color="primary"
              min={0}
              max={this.maxBlurRadius}
              value={this.boxShadowProperties.get('blurRadius')}
              mode="md"
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBoxShadow($event, 'blurRadius')}></ion-range>
          </ion-item>
          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Spread radius <small>{this.boxShadowProperties.get('spreadRadius')}px</small>
            </ion-label>
          </ion-item-divider>
          <ion-item class="item-opacity">
            <ion-range
              color="primary"
              min={-this.maxVLength}
              max={this.maxVLength}
              value={this.boxShadowProperties.get('spreadRadius')}
              mode="md"
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBoxShadow($event, 'spreadRadius')}></ion-range>
          </ion-item>
          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Opacity <small>{this.boxShadowProperties.get('opacity')}</small>
            </ion-label>
          </ion-item-divider>
          <ion-item class="item-opacity">
            <ion-range
              color="primary"
              min={0}
              max={100}
              value={this.boxShadowProperties.get('opacity')}
              mode="md"
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBoxShadow($event, 'opacity')}></ion-range>
          </ion-item>

          <deckgo-color
            class="ion-padding-bottom"
            onColorChange={($event: CustomEvent) => this.selectColor($event, this.setCodeColor)}
            color-rgb={this.color}
            more>
            <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
          </deckgo-color>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
