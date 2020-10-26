import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {ColorUtils, InitStyleColor} from '../../../../../utils/editor/color.utils';

enum ApplyColorType {
  FONT,
  BACKGROUND,
}

@Component({
  tag: 'app-color-sides',
})
export class AppColorSides {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  template: 'split' | 'author';

  @State()
  private applyColorType: ApplyColorType = ApplyColorType.FONT;

  @Event() colorChange: EventEmitter<void>;

  @State()
  private endSide: boolean = false;

  private colorRef!: HTMLAppColorElement;

  private initColor = async (): Promise<InitStyleColor> => {
    if (!this.selectedElement) {
      return {
        rgb: null,
        opacity: null,
      };
    }

    if (this.applyColorType === ApplyColorType.BACKGROUND) {
      return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue(`--slide-${this.template}-background-${this.endSide ? 'end' : 'start'}`));
    } else {
      return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue(`--slide-${this.template}-color-${this.endSide ? 'end' : 'start'}`));
    }
  };

  private async selectApplyType($event: CustomEvent) {
    if ($event && $event.detail) {
      this.applyColorType = $event.detail.value;

      await this.colorRef?.loadColor();
    }
  }

  private async resetColor() {
    if (!this.selectedElement) {
      return;
    }

    if (this.applyColorType === ApplyColorType.BACKGROUND) {
      this.selectedElement.style.removeProperty(`--slide-${this.template}-background-${this.endSide ? 'end' : 'start'}`);
    } else {
      this.selectedElement.style.removeProperty(`--slide-${this.template}-color-${this.endSide ? 'end' : 'start'}`);
    }

    this.colorChange.emit();
  }

  private async applyColor($event: CustomEvent<string>) {
    if (!this.selectedElement || !$event) {
      return;
    }

    if (this.applyColorType === ApplyColorType.BACKGROUND) {
      this.selectedElement.style.setProperty(`--slide-${this.template}-background-${this.endSide ? 'end' : 'start'}`, $event.detail);
    } else {
      this.selectedElement.style.setProperty(`--slide-${this.template}-color-${this.endSide ? 'end' : 'start'}`, $event.detail);
    }

    this.colorChange.emit();
  }

  private async toggleSide() {
    this.endSide = !this.endSide;

    await this.colorRef?.loadColor();
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Colors</ion-label>

        <ion-list>
          <ion-item class="select">
            <ion-label>Apply a color to</ion-label>

            <ion-select
              value={this.applyColorType}
              placeholder="Apply a color to"
              onIonChange={($event: CustomEvent) => this.selectApplyType($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              <ion-select-option value={ApplyColorType.FONT}>Font</ion-select-option>,
              <ion-select-option value={ApplyColorType.BACKGROUND}>Background</ion-select-option>,
            </ion-select>
          </ion-item>

          <ion-radio-group value={this.endSide} onIonChange={() => this.toggleSide()} class="inline ion-padding-start ion-padding-end">
            <ion-item>
              <ion-radio value={false} mode="md"></ion-radio>
              <ion-label>Start</ion-label>
            </ion-item>

            <ion-item>
              <ion-radio value={true} mode="md"></ion-radio>
              <ion-label>End</ion-label>
            </ion-item>
          </ion-radio-group>
        </ion-list>

        <app-color
          class="ion-margin-top"
          ref={(el) => (this.colorRef = el as HTMLAppColorElement)}
          initColor={this.initColor}
          onResetColor={() => this.resetColor()}
          onColorDidChange={($event: CustomEvent<string>) => this.applyColor($event)}></app-color>
      </app-expansion-panel>
    );
  }
}
