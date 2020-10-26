import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {SlideChartType} from '../../../../../models/data/slide';

import {ColorUtils, InitStyleColor} from '../../../../../utils/editor/color.utils';
import {ChartUtils} from '../../../../../utils/editor/chart.utils';

enum ApplyColorType {
  FILL,
  STROKE,
  FONT,
  AXIS,
  GRID,
}

@Component({
  tag: 'app-color-chart',
})
export class AppColorDeckSlide {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @State()
  private applyColorType: ApplyColorType = ApplyColorType.FILL;

  @State()
  private colorIndex: number = 1;

  @State()
  private chartType: SlideChartType = undefined;

  private indexes: number[] = [...Array(99).keys()];

  @Event() colorChange: EventEmitter<void>;

  private colorRef!: HTMLAppColorElement;

  async componentWillLoad() {
    this.chartType = await ChartUtils.initSlideChartType(this.selectedElement);
  }

  private initColor = async (): Promise<InitStyleColor> => {
    if (!this.selectedElement) {
      return {
        rgb: null,
        opacity: null,
      };
    }

    if (this.applyColorType === ApplyColorType.FILL) {
      return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue(`--deckgo-chart-fill-color-${this.colorIndex}`));
    } else if (this.applyColorType === ApplyColorType.STROKE) {
      return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue(`--deckgo-chart-stroke-${this.colorIndex}`));
    } else if (this.applyColorType === ApplyColorType.AXIS) {
      return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--deckgo-chart-axis-color'));
    } else if (this.applyColorType === ApplyColorType.GRID) {
      return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--deckgo-chart-grid-stroke'));
    } else {
      return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--deckgo-chart-text-color'));
    }
  };

  private async toggleColorType($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    this.applyColorType = $event.detail.value;

    await this.colorRef?.loadColor();
  }

  private async applyColor($event: CustomEvent<string>) {
    if (!this.selectedElement || !$event) {
      return;
    }

    const selectedColor: string = $event.detail;

    if (this.applyColorType === ApplyColorType.FILL) {
      this.selectedElement.style.setProperty(`--deckgo-chart-fill-color-${this.colorIndex}`, selectedColor);
    } else if (this.applyColorType === ApplyColorType.STROKE) {
      this.selectedElement.style.setProperty(`--deckgo-chart-stroke-${this.colorIndex}`, selectedColor);
    } else if (this.applyColorType === ApplyColorType.AXIS) {
      this.selectedElement.style.setProperty('--deckgo-chart-axis-color', selectedColor);
    } else if (this.applyColorType === ApplyColorType.GRID) {
      this.selectedElement.style.setProperty('--deckgo-chart-grid-stroke', selectedColor);
    } else {
      this.selectedElement.style.setProperty('--deckgo-chart-text-color', selectedColor);
    }

    this.colorChange.emit();
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

  private async resetColor() {
    if (!this.selectedElement) {
      return;
    }

    if (this.applyColorType === ApplyColorType.FILL) {
      this.selectedElement.style.removeProperty(`--deckgo-chart-fill-color-${this.colorIndex}`);
    } else if (this.applyColorType === ApplyColorType.STROKE) {
      this.selectedElement.style.removeProperty(`--deckgo-chart-stroke-${this.colorIndex}`);
    } else if (this.applyColorType === ApplyColorType.AXIS) {
      this.selectedElement.style.removeProperty('--deckgo-chart-axis-color');
    } else if (this.applyColorType === ApplyColorType.GRID) {
      this.selectedElement.style.removeProperty('--deckgo-chart-grid-stroke');
    } else {
      this.selectedElement.style.removeProperty('--deckgo-chart-text-color');
    }

    this.colorChange.emit();
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Colors</ion-label>

        <ion-list>
          <ion-item-divider class="ion-padding-top">
            <ion-label>Apply a color to</ion-label>
          </ion-item-divider>

          <ion-item class="select">
            <ion-label>Apply a color to</ion-label>

            <ion-select
              value={this.applyColorType}
              placeholder="Apply a color to"
              onIonChange={(e: CustomEvent) => this.toggleColorType(e)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              {this.renderColorOptions()}
            </ion-select>
          </ion-item>

          <ion-item-divider class="ion-padding-top">
            <ion-label>Series</ion-label>
          </ion-item-divider>

          <ion-item class="select">
            <ion-label>Series</ion-label>

            <ion-select
              value={this.colorIndex}
              placeholder="Series index"
              disabled={this.applyColorType !== ApplyColorType.FILL && this.applyColorType !== ApplyColorType.STROKE}
              onIonChange={(e: CustomEvent) => this.selectColorIndex(e)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              {this.renderChartIndexes()}
            </ion-select>
          </ion-item>
        </ion-list>

        <ion-item-divider class="ion-padding-top">
          <ion-label>Color</ion-label>
        </ion-item-divider>

        <app-color
          ref={(el) => (this.colorRef = el as HTMLAppColorElement)}
          initColor={this.initColor}
          onResetColor={() => this.resetColor()}
          onColorDidChange={($event: CustomEvent<string>) => this.applyColor($event)}></app-color>
      </app-expansion-panel>
    );
  }

  private renderColorOptions() {
    const options = [
      <ion-select-option value={ApplyColorType.FILL}>Fill</ion-select-option>,
      <ion-select-option value={ApplyColorType.STROKE}>Stroke</ion-select-option>,
      <ion-select-option value={ApplyColorType.FONT}>Font</ion-select-option>,
    ];

    if (this.chartType != SlideChartType.PIE) {
      options.push(<ion-select-option value={ApplyColorType.AXIS}>Axis</ion-select-option>);
      options.push(<ion-select-option value={ApplyColorType.GRID}>Grid</ion-select-option>);
    }

    return options;
  }

  // A select is more user friendly than an input
  private renderChartIndexes() {
    return this.indexes.map((index: number) => {
      return <ion-select-option value={index + 1}>{`${index + 1}`}</ion-select-option>;
    });
  }
}
