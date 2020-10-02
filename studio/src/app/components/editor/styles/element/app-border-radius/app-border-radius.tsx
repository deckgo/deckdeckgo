import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

@Component({
  tag: 'app-border-radius',
})
export class BorderRadius {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private borderRadiuses: Map<string, number> = new Map([
    ['General', 2],
    ['TopLeft', 2],
    ['TopRight', 2],
    ['BottomLeft', 2],
    ['BottomRight', 2],
  ]);

  private readonly maxBorderRadius: number = 64;

  @Event() borderRadiusDidChange: EventEmitter<void>;

  async componentWillLoad() {
    await this.initBorderRadius();
  }

  private async initBorderRadius() {
    if (!this.selectedElement || !window) {
      return;
    }

    const style: CSSStyleDeclaration = window.getComputedStyle(this.selectedElement);

    if (!style) {
      return;
    }

    this.borderRadiuses.set('TopLeft', parseInt(style.borderTopLeftRadius));
    this.borderRadiuses.set('TopRight', parseInt(style.borderTopRightRadius));
    this.borderRadiuses.set('BottomRight', parseInt(style.borderBottomRightRadius));
    this.borderRadiuses.set('BottomLeft', parseInt(style.borderBottomLeftRadius));
  }

  private emitBorderRadiusChange() {
    this.borderRadiusDidChange.emit();
  }

  private async updateBorderRadius($event: CustomEvent, corner: string = ''): Promise<void> {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }
    if (corner === 'General') {
      this.borderRadiuses.forEach((_, key) => {
        this.borderRadiuses.set(key, $event.detail.value);
      });
      this.selectedElement.style.borderRadius = `${$event.detail.value}px`;
    } else {
      this.borderRadiuses.set(corner, $event.detail.value);
      this.selectedElement.style[`border${corner}Radius`] = `${$event.detail.value}px`;
    }
    this.borderRadiuses = new Map<string, number>(this.borderRadiuses);

    this.emitBorderRadiusChange();
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Border radius</ion-label>
        <ion-list>
          {this.renderOption('General', 'Every corner')}
          {this.renderOption('TopLeft', 'Top left')}
          {this.renderOption('TopRight', 'Top right')}
          {this.renderOption('BottomRight', 'Bottom right')}
          {this.renderOption('BottomLeft', 'Bottom left')}
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderOption(option: 'General' | 'TopLeft' | 'TopRight' | 'BottomRight' | 'BottomLeft', text: string) {
    const borderRadius: number = this.borderRadiuses.get(option);

    return [
      <ion-item-divider class="ion-padding-top">
        <ion-label>
          {text} {borderRadius > 2 ? <small>{borderRadius}px</small> : undefined}
        </ion-label>
      </ion-item-divider>,
      <ion-item class="item-opacity">
        <ion-range
          color="primary"
          min={2}
          max={this.maxBorderRadius}
          value={this.borderRadiuses.get(option)}
          mode="md"
          onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBorderRadius($event, option)}></ion-range>
      </ion-item>,
    ];
  }
}
