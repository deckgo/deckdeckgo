import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import i18n from '../../../../../../stores/i18n.store';

import {ColorUtils, InitStyleColor} from '../../../../../../utils/editor/color.utils';

enum ApplyColorType {
  FONT,
  BACKGROUND
}

enum ApplyColorSide {
  START = 'start',
  END = 'end'
}

@Component({
  tag: 'app-color-sides'
})
export class AppColorSides {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

  @Prop()
  template: 'split' | 'author';

  @State()
  private applyColorType: ApplyColorType = ApplyColorType.FONT;

  @State()
  private side: ApplyColorSide = ApplyColorSide.START;

  @Event() colorChange: EventEmitter<void>;

  private colorRef!: HTMLAppColorElement;

  private initColor = async (): Promise<InitStyleColor> => {
    if (!this.selectedTarget) {
      return {
        rgb: null,
        opacity: null
      };
    }

    if (this.applyColorType === ApplyColorType.BACKGROUND) {
      return ColorUtils.splitColor(this.selectedTarget.style.getPropertyValue(`--slide-${this.template}-background-${this.side}`));
    } else {
      return ColorUtils.splitColor(this.selectedTarget.style.getPropertyValue(`--slide-${this.template}-color-${this.side}`));
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

    if (this.applyColorType === ApplyColorType.BACKGROUND) {
      this.selectedTarget.style.removeProperty(`--slide-${this.template}-background-${this.side}`);
    } else {
      this.selectedTarget.style.removeProperty(`--slide-${this.template}-color-${this.side}`);
    }

    this.colorChange.emit();
  }

  private async applyColor($event: CustomEvent<string>) {
    if (!this.selectedTarget || !$event) {
      return;
    }

    if (this.applyColorType === ApplyColorType.BACKGROUND) {
      this.selectedTarget.style.setProperty(`--slide-${this.template}-background-${this.side}`, $event.detail);
    } else {
      this.selectedTarget.style.setProperty(`--slide-${this.template}-color-${this.side}`, $event.detail);
    }

    this.colorChange.emit();
  }

  private async toggleSide($event: CustomEvent) {
    if ($event && $event.detail) {
      this.side = $event.detail.value;

      await this.colorRef?.loadColor();
    }
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">{i18n.state.editor.colors}</ion-label>

        <ion-list>
          <ion-item-divider class="ion-padding-top">
            <ion-label>{i18n.state.editor.apply_a_color_to}</ion-label>
          </ion-item-divider>

          <ion-item class="select">
            <ion-label>{i18n.state.editor.apply_a_color_to}</ion-label>

            <ion-select
              value={this.applyColorType}
              placeholder={i18n.state.editor.apply_a_color_to}
              onIonChange={($event: CustomEvent) => this.selectApplyType($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end"
            >
              <ion-select-option value={ApplyColorType.FONT}>{i18n.state.editor.font}</ion-select-option>
              <ion-select-option value={ApplyColorType.BACKGROUND}>{i18n.state.editor.background}</ion-select-option>
            </ion-select>
          </ion-item>

          <ion-item-divider class="ion-padding-top">
            <ion-label>{i18n.state.editor.side}</ion-label>
          </ion-item-divider>

          <ion-item class="select">
            <ion-label>{i18n.state.editor.side}</ion-label>

            <ion-select
              value={this.side}
              placeholder={i18n.state.editor.side}
              onIonChange={($event: CustomEvent) => this.toggleSide($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end"
            >
              <ion-select-option value={ApplyColorSide.START}>{i18n.state.editor.start}</ion-select-option>
              <ion-select-option value={ApplyColorSide.END}>{i18n.state.editor.end}</ion-select-option>
            </ion-select>
          </ion-item>
        </ion-list>

        <ion-item-divider class="ion-padding-top">
          <ion-label>{i18n.state.editor.color}</ion-label>
        </ion-item-divider>

        <app-color
          ref={(el) => (this.colorRef = el as HTMLAppColorElement)}
          initColor={this.initColor}
          onResetColor={() => this.resetColor()}
          onColorDidChange={($event: CustomEvent<string>) => this.applyColor($event)}
        ></app-color>
      </app-expansion-panel>
    );
  }
}
