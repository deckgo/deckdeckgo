import {Component, Element, Event, EventEmitter, h, Method, Prop, State, Watch} from '@stencil/core';

import {ColorUtils, InitStyleColor} from '../../../../utils/editor/color.utils';

@Component({
  tag: 'app-color-text-background',
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

  @Prop()
  colorType: 'text' | 'background' = 'text';

  @Prop()
  expanded: boolean = true;

  @State()
  private color: string;

  @State()
  private colorOpacity: number = 100;

  @Event() colorChange: EventEmitter<void>;

  async componentWillLoad() {
    await this.initCurrentColors();
  }

  @Watch('colorType')
  async onColorTypeChange() {
    await this.initCurrentColors();
  }

  @Method()
  async initCurrentColors() {
    if (!this.selectedElement) {
      return;
    }

    let styleColor: InitStyleColor;

    // prettier-ignore
    if (this.colorType === 'background') {
      styleColor = await ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--background') ? this.selectedElement.style.getPropertyValue('--background') : this.selectedElement.style.background);
    } else {
      styleColor = await ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--color') ? this.selectedElement.style.getPropertyValue('--color') : this.selectedElement.style.color);
    }

    this.color = styleColor.rgb;
    this.colorOpacity = styleColor.opacity;
  }

  private async applyColor() {
    if (this.colorType === 'background') {
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

      if (this.colorType === 'background') {
        this.selectedElement.style.removeProperty('--background');
        this.selectedElement.style.removeProperty('background');
      } else {
        this.selectedElement.style.removeProperty('--color');
        this.selectedElement.style.removeProperty('color');
      }

      this.color = null;
      this.colorOpacity = 100;

      this.colorChange.emit();

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

      this.colorChange.emit();

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

      this.colorChange.emit();

      resolve();
    });
  }

  render() {
    return (
      <app-expansion-panel expanded={this.expanded ? 'open' : 'close'}>
        <ion-label slot="title">Color</ion-label>

        <app-color onResetColor={() => this.resetColor()} onColorDidChange={() => this.applyColor()}></app-color>
      </app-expansion-panel>
    );
  }
}
