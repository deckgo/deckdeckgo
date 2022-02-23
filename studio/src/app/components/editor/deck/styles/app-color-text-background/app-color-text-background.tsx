import {Component, Element, Event, EventEmitter, h, Prop} from '@stencil/core';
import i18n from '../../../../../stores/i18n.store';
import settingsStore from '../../../../../stores/settings.store';
import {Expanded} from '../../../../../types/core/settings';
import {SettingsUtils} from '../../../../../utils/core/settings.utils';
import {ColorUtils, InitStyleColor} from '../../../../../utils/editor/color.utils';
import {setStyle} from '../../../../../utils/editor/undo-redo.deck.utils';

@Component({
  tag: 'app-color-text-background'
})
export class AppColorTextBackground {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

  @Prop()
  slide: boolean = false;

  @Prop()
  deck: boolean = false;

  @Prop()
  colorType: 'text' | 'background' = 'text';

  private colorRef!: HTMLAppColorElement;

  @Event() colorChange: EventEmitter<void>;

  private initBackground = async (): Promise<InitStyleColor> => {
    if (!this.selectedTarget) {
      return {
        rgb: null,
        opacity: null
      };
    }

    return ColorUtils.splitColor(
      this.selectedTarget.style.getPropertyValue('--background')
        ? this.selectedTarget.style.getPropertyValue('--background')
        : this.selectedTarget.style.background
    );
  };

  private initColor = async (): Promise<InitStyleColor> => {
    if (!this.selectedTarget) {
      return {
        rgb: null,
        opacity: null
      };
    }

    return ColorUtils.splitColor(
      this.selectedTarget.style.getPropertyValue('--color')
        ? this.selectedTarget.style.getPropertyValue('--color')
        : this.selectedTarget.style.color
    );
  };

  private async applyColor($event: CustomEvent<string>) {
    if (this.colorType === 'background') {
      await this.applyBackground($event?.detail);
    } else {
      await this.applyTextColor($event?.detail);
    }
  }

  private async resetColor() {
    if (!this.selectedTarget) {
      return;
    }

    if (this.colorType === 'background') {
      this.selectedTarget.style.removeProperty('--background');
      this.selectedTarget.style.removeProperty('background');
    } else {
      this.selectedTarget.style.removeProperty('--color');
      this.selectedTarget.style.removeProperty('color');
    }

    this.colorChange.emit();
  }

  private async applyTextColor(selectedColor: string) {
    if (!this.selectedTarget || !selectedColor) {
      return;
    }

    setStyle(this.selectedTarget, {
      properties: [{property: this.deck || this.slide ? '--color' : 'color', value: selectedColor}],
      type: this.deck ? 'deck' : this.slide ? 'slide' : 'element',
      updateUI: async (_value: string) => await this.colorRef.loadColor()
    });

    this.colorChange.emit();
  }

  private async applyBackground(selectedColor: string) {
    if (!this.selectedTarget || !selectedColor) {
      return;
    }

    setStyle(this.selectedTarget, {
      properties: [{value: selectedColor, property: this.deck || this.slide ? '--background' : 'background'}],
      type: this.deck ? 'deck' : this.slide ? 'slide' : 'element',
      updateUI: async (_value: string) => await this.colorRef.loadColor()
    });

    this.colorChange.emit();
  }

  render() {
    return (
      <app-expansion-panel
        expanded={this.colorType === 'text' ? settingsStore.state.panels.color : settingsStore.state.panels.background}
        onExpansion={($event: CustomEvent<Expanded>) =>
          SettingsUtils.update(this.colorType === 'text' ? {color: $event.detail} : {background: $event.detail})
        }
      >
        <ion-label slot="title">{i18n.state.editor.color}</ion-label>

        <app-color
          ref={(el) => (this.colorRef = el as HTMLAppColorElement)}
          initColor={this.colorType === 'background' ? this.initBackground : this.initColor}
          onResetColor={() => this.resetColor()}
          defaultColor={this.colorType === 'background' ? '#fff' : '#000'}
          onColorDidChange={($event: CustomEvent<string>) => this.applyColor($event)}
        ></app-color>
      </app-expansion-panel>
    );
  }
}
