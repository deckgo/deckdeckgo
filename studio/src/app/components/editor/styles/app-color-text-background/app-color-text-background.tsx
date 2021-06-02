import {Component, Element, Event, EventEmitter, h, Prop} from '@stencil/core';

import settingsStore from '../../../../stores/settings.store';
import i18n from '../../../../stores/i18n.store';

import {ColorUtils, InitStyleColor} from '../../../../utils/editor/color.utils';
import {SettingsUtils} from '../../../../utils/core/settings.utils';
import {setStyle} from '../../../../helpers/editor/editor.helper';

import {Expanded} from '../../../../types/core/settings';

@Component({
  tag: 'app-color-text-background',
})
export class AppColorTextBackground {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  slide: boolean = false;

  @Prop()
  deck: boolean = false;

  @Prop()
  colorType: 'text' | 'background' = 'text';

  private colorRef!: HTMLAppColorElement;

  @Event() colorChange: EventEmitter<void>;

  private initBackground = async (): Promise<InitStyleColor> => {
    if (!this.selectedElement) {
      return {
        rgb: null,
        opacity: null,
      };
    }

    return ColorUtils.splitColor(
      this.selectedElement.style.getPropertyValue('--background')
        ? this.selectedElement.style.getPropertyValue('--background')
        : this.selectedElement.style.background
    );
  };

  private initColor = async (): Promise<InitStyleColor> => {
    if (!this.selectedElement) {
      return {
        rgb: null,
        opacity: null,
      };
    }

    return ColorUtils.splitColor(
      this.selectedElement.style.getPropertyValue('--color') ? this.selectedElement.style.getPropertyValue('--color') : this.selectedElement.style.color
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
    if (!this.selectedElement) {
      return;
    }

    if (this.colorType === 'background') {
      this.selectedElement.style.removeProperty('--background');
      this.selectedElement.style.removeProperty('background');
    } else {
      this.selectedElement.style.removeProperty('--color');
      this.selectedElement.style.removeProperty('color');
    }

    this.colorChange.emit();
  }

  private async applyTextColor(selectedColor: string) {
    if (!this.selectedElement || !selectedColor) {
      return;
    }

    setStyle(this.selectedElement, this.deck || this.slide ? '--color' : 'color', {
      value: selectedColor,
      updateUI: async (_value: string) => await this.colorRef.loadColor()
    });

    this.colorChange.emit();
  }

  private async applyBackground(selectedColor: string) {
    if (!this.selectedElement || !selectedColor) {
      return;
    }

    setStyle(this.selectedElement, this.deck || this.slide ? '--background' : 'background', {
      value: selectedColor,
      updateUI: async (_value: string) => await this.colorRef.loadColor()
    });

    this.colorChange.emit();
  }

  render() {
    return (
      <app-expansion-panel
        expanded={this.colorType === 'text' ? settingsStore.state.panels.color : settingsStore.state.panels.background}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update(this.colorType === 'text' ? {color: $event.detail} : {background: $event.detail})}>
        <ion-label slot="title">{i18n.state.editor.color}</ion-label>

        <app-color ref={(el) => this.colorRef = el as HTMLAppColorElement}
          initColor={this.colorType === 'background' ? this.initBackground : this.initColor}
          onResetColor={() => this.resetColor()}
          defaultColor={this.colorType === 'background' ? '#fff' : '#000'}
          onColorDidChange={($event: CustomEvent<string>) => this.applyColor($event)}></app-color>
      </app-expansion-panel>
    );
  }
}
