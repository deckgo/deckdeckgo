import {Component, Element, Event, EventEmitter, h, Method, Prop, State} from '@stencil/core';
import {RangeChangeEventDetail} from '@ionic/core';

import {ColorUtils, InitStyleColor} from '../../../../utils/editor/color.utils';

enum ApplyColorType {
  QR_CODE,
  BACKGROUND,
}

@Component({
  tag: 'app-color-qrcode',
})
export class AppColorQRCode {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  moreColors: boolean = true;

  @State()
  private color: string;

  @State()
  private colorOpacity: number = 100;

  @State()
  private applyColorType: ApplyColorType = ApplyColorType.QR_CODE;

  @Event() colorChange: EventEmitter<boolean>;

  async componentWillLoad() {
    await this.initCurrentColors();
  }

  @Method()
  async initCurrentColors() {
    if (!this.selectedElement) {
      return;
    }

    const element: HTMLElement = this.selectedElement;

    if (!element) {
      return;
    }

    let styleColor: InitStyleColor;

    if (this.applyColorType === ApplyColorType.BACKGROUND) {
      styleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--deckgo-qrcode-background-fill'));
    } else {
      styleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--deckgo-qrcode-color-fill'));
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

  private resetColor(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (this.applyColorType === ApplyColorType.BACKGROUND) {
        this.selectedElement.style.removeProperty('--deckgo-qrcode-background-fill');
      } else {
        this.selectedElement.style.removeProperty('--deckgo-qrcode-color-fill');
      }

      this.color = null;
      this.colorOpacity = 100;

      this.colorChange.emit(false);

      resolve();
    });
  }

  private applyColor(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.color) {
        resolve();
        return;
      }

      const selectedColor: string = `rgba(${this.color},${ColorUtils.transformOpacity(this.colorOpacity)})`;

      if (this.applyColorType === ApplyColorType.BACKGROUND) {
        this.selectedElement.style.setProperty('--deckgo-qrcode-background-fill', selectedColor);
      } else {
        this.selectedElement.style.setProperty('--deckgo-qrcode-color-fill', selectedColor);
      }

      this.colorChange.emit(false);

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
        <ion-radio-group onIonChange={($event) => this.selectApplyType($event)} value={ApplyColorType.QR_CODE}>
          <ion-item-divider class="ion-padding-top">
            <ion-label>Apply color to</ion-label>
          </ion-item-divider>

          <ion-item>
            <ion-label>Fill</ion-label>
            <ion-radio slot="start" value={ApplyColorType.QR_CODE} mode="md"></ion-radio>
          </ion-item>

          <ion-item>
            <ion-label>Background</ion-label>
            <ion-radio slot="start" value={ApplyColorType.BACKGROUND} mode="md"></ion-radio>
          </ion-item>
        </ion-radio-group>
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
      </ion-list>,
      <deckgo-color
        class="ion-padding-start ion-padding-end ion-padding-bottom"
        more={this.moreColors}
        onColorChange={($event: CustomEvent) => this.selectColor($event)}
        color-rgb={this.color}>
        <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
      </deckgo-color>,
      <ion-item class="action-button ion-margin-bottom">
        <ion-button shape="round" onClick={() => this.resetColor()} fill="outline" class="delete">
          <ion-label>{this.resetLabelContent()}</ion-label>
        </ion-button>
      </ion-item>,
    ];
  }

  private resetLabelContent() {
    if (this.applyColorType === ApplyColorType.QR_CODE) {
      return 'Reset fill color';
    } else {
      return 'Reset background';
    }
  }
}
