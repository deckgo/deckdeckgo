import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {RangeChangeEventDetail} from '@ionic/core';

@Component({
  tag: 'app-border-radius',
  styleUrl: 'app-align.scss',
})
export class BorderRadius {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private borderRadiuses: Map<string, number> = new Map([
    ['General', 0],
    ['TopLeft', 0],
    ['TopRight', 0],
    ['BottomLeft', 0],
    ['BottomRight', 0],
  ]);

  @State()
  private maxBorderRadius: number = 0;

  @Event() borderRadiusDidChange: EventEmitter<void>;

  async componentWillLoad() {
    if (this.selectedElement) {
      this.maxBorderRadius = this.selectedElement.offsetHeight / 2;
    }
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
          {this.renderOption('General', 'Border radius')}
          {this.renderOption('TopLeft', 'Top left')}
          {this.renderOption('TopRight', 'Top right')}
          {this.renderOption('BottomRight', 'Bottom right')}
          {this.renderOption('BottomLeft', 'Bottom left')}
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderOption(option: 'General' | 'TopLeft' | 'TopRight' | 'BottomRight' | 'BottomLeft', text: string) {
    return [
      <ion-item-divider class="ion-padding-top">
        <ion-label>
          {text} <small>{this.borderRadiuses.get(option)}px</small>
        </ion-label>
      </ion-item-divider>,
      <ion-item class="item-opacity">
        <ion-range
          color="primary"
          min={0}
          max={this.maxBorderRadius}
          value={this.borderRadiuses.get(option)}
          mode="md"
          onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateBorderRadius($event, option)}></ion-range>
      </ion-item>,
    ];
  }
}
