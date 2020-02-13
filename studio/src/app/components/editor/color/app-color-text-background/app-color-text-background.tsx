import {Component, Element, Event, EventEmitter, h, Method, Prop, State} from '@stencil/core';
import {RangeChangeEventDetail} from '@ionic/core';

import {ColorUtils, InitStyleColor} from '../../../../utils/editor/color.utils';

enum ApplyColorType {
  TEXT,
  BACKGROUND
}

@Component({
  tag: 'app-color-text-background'
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

  @State()
  private color: string;

  @State()
  private colorOpacity: number = 100;

  @State()
  private applyColorType: ApplyColorType = ApplyColorType.TEXT;

  @Event() colorChange: EventEmitter<boolean>;

  async componentWillLoad() {
    await this.initCurrentColors();
  }

  @Method()
  async initCurrentColors() {
    if (!this.selectedElement) {
      return;
    }

    let styleColor: InitStyleColor;

    // prettier-ignore
    if (this.applyColorType === ApplyColorType.BACKGROUND) {
      styleColor = await ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--background') ? this.selectedElement.style.getPropertyValue('--background') : this.selectedElement.style.background);
    } else {
      styleColor = await ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--color') ? this.selectedElement.style.getPropertyValue('--color') : this.selectedElement.style.color);
    }

    this.color = styleColor.rgb;
    this.colorOpacity = styleColor.opacity;
  }

  private async selectApplyType($event: CustomEvent) {
    if ($event && $event.detail) {
      this.applyColorType = $event.detail.value;

      await this.initCurrentColors();
    }
  }

  private async selectColor($event: CustomEvent) {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }

    this.color = $event.detail.rgb;

    await this.applyColor();
  }

  private async applyColor() {
    if (this.applyColorType === ApplyColorType.BACKGROUND) {
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

      if (this.applyColorType === ApplyColorType.BACKGROUND) {
        this.selectedElement.style.removeProperty('--background');
        this.selectedElement.style.removeProperty('background');
      } else {
        this.selectedElement.style.removeProperty('--color');
        this.selectedElement.style.removeProperty('color');
      }

      this.color = null;
      this.colorOpacity = 100;

      this.colorChange.emit(this.deck);

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

      this.colorChange.emit(this.deck);

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

      this.colorChange.emit(this.deck);

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
    return [
      <ion-list>
        <ion-radio-group onIonChange={($event) => this.selectApplyType($event)} value={ApplyColorType.TEXT}>
          <ion-item-divider class="ion-padding-top">
            <ion-label>Apply color to</ion-label>
          </ion-item-divider>

          <ion-item>
            <ion-label>Text</ion-label>
            <ion-radio slot="start" value={ApplyColorType.TEXT} mode="md"></ion-radio>
          </ion-item>

          <ion-item>
            <ion-label>Background</ion-label>
            <ion-radio slot="start" value={ApplyColorType.BACKGROUND} mode="md"></ion-radio>
          </ion-item>
        </ion-radio-group>
        <ion-item-divider class="ion-padding-top">
          <ion-label>Opacity</ion-label>
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
      </ion-list>,
      <deckgo-color
        class="ion-padding-start ion-padding-end ion-padding-bottom"
        more={this.moreColors}
        onColorChange={($event: CustomEvent) => this.selectColor($event)}
        color-rgb={this.color}>
        <ion-icon src="/assets/icons/ionicons/md-more.svg" slot="more" aria-label="More" class="more"></ion-icon>
      </deckgo-color>,
      <ion-item class="action-button ion-margin-bottom">
        <ion-button shape="round" onClick={() => this.resetColor()} fill="outline" class="delete">
          <ion-label class="ion-text-uppercase">{this.resetLabelContent()}</ion-label>
        </ion-button>
      </ion-item>
    ];
  }

  private resetLabelContent() {
    if (this.applyColorType === ApplyColorType.BACKGROUND) {
      return 'Reset background';
    } else {
      return 'Reset color';
    }
  }
}
