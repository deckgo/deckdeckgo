import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import i18n from '../../../../../../stores/i18n.store';

import {ColorUtils, InitStyleColor} from '../../../../../../utils/editor/color.utils';

enum ApplyColorType {
  QR_CODE,
  BACKDROP
}

@Component({
  tag: 'app-color-qrcode'
})
export class AppColorQRCode {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

  @State()
  private applyColorType: ApplyColorType = ApplyColorType.QR_CODE;

  @Event() colorChange: EventEmitter<void>;

  private colorRef!: HTMLAppColorElement;

  private initColor = async (): Promise<InitStyleColor> => {
    if (!this.selectedTarget) {
      return {
        rgb: null,
        opacity: null
      };
    }

    if (this.applyColorType === ApplyColorType.BACKDROP) {
      return ColorUtils.splitColor(this.selectedTarget.style.getPropertyValue('--deckgo-qrcode-background-fill'));
    } else {
      return ColorUtils.splitColor(this.selectedTarget.style.getPropertyValue('--deckgo-qrcode-color-fill'));
    }
  };

  private async selectApplyType($event: CustomEvent) {
    if ($event && $event.detail) {
      this.applyColorType = $event.detail.value;

      await this.colorRef?.loadColor();
    }
  }

  private async resetColor() {
    if (!this.selectedTarget) {
      return;
    }

    if (this.applyColorType === ApplyColorType.BACKDROP) {
      this.selectedTarget.style.removeProperty('--deckgo-qrcode-background-fill');
    } else {
      this.selectedTarget.style.removeProperty('--deckgo-qrcode-color-fill');
    }

    this.colorChange.emit();
  }

  private async applyColor($event: CustomEvent<string>) {
    if (!this.selectedTarget || !$event) {
      return;
    }

    if (this.applyColorType === ApplyColorType.BACKDROP) {
      this.selectedTarget.style.setProperty('--deckgo-qrcode-background-fill', $event.detail);
    } else {
      this.selectedTarget.style.setProperty('--deckgo-qrcode-color-fill', $event.detail);
    }

    this.colorChange.emit();
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">{i18n.state.editor.colors}</ion-label>

        <ion-list>
          <ion-item class="select">
            <ion-label>{i18n.state.editor.apply_a_color_to}</ion-label>

            <ion-select
              value={this.applyColorType}
              placeholder={i18n.state.editor.apply_a_color_to}
              onIonChange={($event: CustomEvent) => this.selectApplyType($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              <ion-select-option value={ApplyColorType.QR_CODE}>{i18n.state.editor.fill}</ion-select-option>,
              <ion-select-option value={ApplyColorType.BACKDROP}>{i18n.state.editor.backdrop}</ion-select-option>,
            </ion-select>
          </ion-item>
        </ion-list>

        <app-color
          ref={(el) => (this.colorRef = el as HTMLAppColorElement)}
          initColor={this.initColor}
          onResetColor={() => this.resetColor()}
          onColorDidChange={($event: CustomEvent<string>) => this.applyColor($event)}></app-color>
      </app-expansion-panel>
    );
  }
}
