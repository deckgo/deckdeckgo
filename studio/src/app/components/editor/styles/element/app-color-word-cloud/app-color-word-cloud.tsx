import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {ColorUtils, InitStyleColor} from '../../../../../utils/editor/color.utils';

@Component({
  tag: 'app-color-word-cloud',
})
export class AppColorWordCloud {
  @Prop()
  selectedElement: HTMLElement;

  @Event() wordCloudDidChange: EventEmitter<void>;

  @State()
  private colorIndex: number = 1;

  private indexes: number[] = [...Array(99).keys()];

  private colorRef!: HTMLAppColorElement;

  private async applyColor($event: CustomEvent<string>) {
    if (!this.selectedElement || !$event) {
      return;
    }

    this.selectedElement.style.setProperty(this.getStyle(), $event.detail);

    this.emitChange();
  }

  private getStyle(): string {
    return `--deckgo-word-count-fill-color-${this.colorIndex}`;
  }

  private initColor = async (): Promise<InitStyleColor> => {
    if (!this.selectedElement) {
      return {
        rgb: null,
        opacity: null,
      };
    }

    return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue(this.getStyle()));
  };

  private emitChange() {
    this.wordCloudDidChange.emit();
  }

  private async resetColor() {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.removeProperty(this.getStyle());

    this.emitChange();
  }

  private async selectColorIndex($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    const input: string = $event.detail.value;

    if (!isNaN(input as any)) {
      this.colorIndex = parseInt(input);
      await this.colorRef?.loadColor();
    }
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Colors</ion-label>

        <ion-list>
          <ion-item class="select">
            <ion-label>Series</ion-label>

            <ion-select
              value={this.colorIndex}
              placeholder="Series index"
              onIonChange={(e: CustomEvent) => this.selectColorIndex(e)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              {this.renderChartIndexes()}
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

  // A select is more user friendly than an input
  private renderChartIndexes() {
    return this.indexes.map((index: number) => {
      return <ion-select-option value={index + 1}>{`Word ${index + 1}`}</ion-select-option>;
    });
  }
}
