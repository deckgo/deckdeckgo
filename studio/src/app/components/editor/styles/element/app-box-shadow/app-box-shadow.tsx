import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

import {ColorUtils, InitStyleColor} from '../../../../../utils/editor/color.utils';

@Component({
  tag: 'app-box-shadow',
})
export class AppBoxShadow {
  @Prop()
  selectedElement: HTMLElement;

  private readonly defaultBoxShadowProperties = new Map([
    ['hLength', 0],
    ['vLength', 4],
    ['blurRadius', 16],
    ['spreadRadius', 0],
  ]);

  @State()
  private boxShadowProperties: Map<string, number> = new Map<string, number>(this.defaultBoxShadowProperties);

  private color: string;

  @State()
  private boxShadow: boolean = false;

  @Event() boxShadowDidChange: EventEmitter<void>;

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
    this.color = rgba && rgba.length > 0 ? rgba[0] : '0, 0, 0';

    const properties: string[] | null = style.boxShadow.split(/rgb.*\)/g);

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

  private initColor = async (): Promise<InitStyleColor> => {
    if (!this.selectedElement) {
      return {
        rgb: null,
        opacity: null,
      };
    }

    const style: CSSStyleDeclaration = window.getComputedStyle(this.selectedElement);

    if (!style) {
      return {
        rgb: null,
        opacity: null,
      };
    }

    if (!style.boxShadow || style.boxShadow === 'none') {
      return {
        rgb: null,
        opacity: null,
      };
    }

    const rgba: string[] | null = style.boxShadow.match(/rgb.*\)/g);

    if (rgba && rgba.length > 0) {
      const styleColor: InitStyleColor = await ColorUtils.splitColor(rgba[0]);

      return {
        rgb: styleColor.rgb ? styleColor.rgb : '0, 0, 0',
        opacity: styleColor.rgb ? styleColor.opacity : 12,
      };
    }

    return {
      rgb: null,
      opacity: null,
    };
  };

  private async selectColor($event: CustomEvent<string>) {
    if (!this.selectedElement) {
      return;
    }

    if (!$event || !$event.detail) {
      return;
    }

    this.color = $event.detail;

    this.boxShadow = true;

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
    this.selectedElement.style.boxShadow = `${this.boxShadowProperties.get('hLength')}px ${this.boxShadowProperties.get(
      'vLength'
    )}px ${this.boxShadowProperties.get('blurRadius')}px ${this.boxShadowProperties.get('spreadRadius')}px ${this.color}`;

    this.emitBoxShadowChange();
  }

  private async resetBoxShadow() {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.boxShadow = '';

    this.boxShadowProperties = new Map<string, number>(this.defaultBoxShadowProperties);

    // Lazy hack, could probably hook on did update and set boxshadow to false afterwards
    // If set directly, components time to time are still enabled
    setTimeout(() => {
      this.boxShadow = false;
    }, 150);

    this.emitBoxShadowChange();
  }

  render() {
    return (
      <app-expansion-panel expanded={'close'}>
        <ion-label slot="title">Box shadow</ion-label>

        <app-color
          class="ion-margin-top"
          history={false}
          initColor={this.initColor}
          onResetColor={() => this.resetBoxShadow()}
          onColorDidChange={($event: CustomEvent<string>) => this.selectColor($event)}></app-color>

        <ion-list>
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
        </ion-list>
      </app-expansion-panel>
    );
  }
}
