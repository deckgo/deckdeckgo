import {Component, Element, Event, EventEmitter, h, Method, Prop, State} from '@stencil/core';
import {RangeChangeEventDetail} from '@ionic/core';

import {SlideChartType} from '../../../../../models/data/slide';

import {ColorUtils, InitStyleColor} from '../../../../../utils/editor/color.utils';
import {ChartUtils} from '../../../../../utils/editor/chart.utils';
import paletteStore from '../../../../../stores/palette.store';
import {colorInPaletteHandler} from '../../../../../helpers/editor/palette.helper';

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

  @Prop()
  moreColors: boolean = true;

  @State()
  private applyColorType: ApplyColorType = ApplyColorType.FILL;

  @State()
  private color: string;

  @State()
  private colorOpacity: number = 100;

  @State()
  private colorIndex: number = 1;

  @State()
  private chartType: SlideChartType = undefined;

  private indexes: number[] = [...Array(99).keys()];

  @Event() colorChange: EventEmitter<void>;

  async componentWillLoad() {
    this.chartType = await ChartUtils.initSlideChartType(this.selectedElement);

    await this.initCurrentColors();
  }

  @Method()
  async initCurrentColors() {
    if (!this.selectedElement) {
      return;
    }

    const element: HTMLElement = this.selectedElement;

    if (!element) {
      return;
    }

    await this.initColor(element);
  }

  private async initColor(element: HTMLElement) {
    let styleColor: InitStyleColor;

    if (this.applyColorType === ApplyColorType.FILL) {
      styleColor = await ColorUtils.splitColor(element.style.getPropertyValue(`--deckgo-chart-fill-color-${this.colorIndex}`));
    } else if (this.applyColorType === ApplyColorType.STROKE) {
      styleColor = await ColorUtils.splitColor(element.style.getPropertyValue(`--deckgo-chart-stroke-${this.colorIndex}`));
    } else if (this.applyColorType === ApplyColorType.AXIS) {
      styleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--deckgo-chart-axis-color'));
    } else if (this.applyColorType === ApplyColorType.GRID) {
      styleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--deckgo-chart-grid-stroke'));
    } else {
      styleColor = await ColorUtils.splitColor(element.style.getPropertyValue('--deckgo-chart-text-color'));
    }

    this.color = styleColor.rgb;
    this.colorOpacity = styleColor.opacity;
  }

  private toggleColorType($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      this.applyColorType = $event.detail.value;

      await this.initCurrentColors();
    });
  }

  private async selectColor($event: CustomEvent) {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }
    paletteStore.state.palette = colorInPaletteHandler(paletteStore.state.palette, $event.detail);

    this.color = $event.detail.rgb;

    await this.applyColor();
  }

  private applyColor(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.color) {
        resolve();
        return;
      }

      const selectedColor: string = `rgba(${this.color},${ColorUtils.transformOpacity(this.colorOpacity)})`;

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

      resolve();
    });
  }

  private updateOpacity($event: CustomEvent<RangeChangeEventDetail>): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
        resolve();
        return;
      }

      $event.stopPropagation();

      const opacity: number = $event.detail.value as number;

      this.colorOpacity = opacity;

      await this.applyColor();

      resolve();
    });
  }

  private selectColorIndex($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      const input: string = $event.detail.value;

      if (!isNaN(input as any)) {
        this.colorIndex = parseInt(input);

        await this.initCurrentColors();
      }
    });
  }

  render() {
    return [
      <ion-item-divider class="ion-padding-top">
        <ion-label>Apply a color to</ion-label>
      </ion-item-divider>,

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
      </ion-item>,

      <ion-item-divider class="ion-padding-top">
        <ion-label>Series</ion-label>
      </ion-item-divider>,

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
      </ion-item>,

      <ion-item-divider class="ion-padding-top">
        <ion-label>
          Opacity <small>{this.colorOpacity}%</small>
        </ion-label>
      </ion-item-divider>,

      <ion-item class="item-opacity">
        <ion-range
          color="primary"
          min={0}
          max={100}
          disabled={!this.color || this.color === undefined}
          value={this.colorOpacity}
          mode="md"
          onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateOpacity(e)}></ion-range>
      </ion-item>,

      <deckgo-color
        palette={paletteStore.state.palette}
        class="ion-padding-start ion-padding-end ion-padding-bottom"
        more={this.moreColors}
        onColorChange={($event: CustomEvent) => this.selectColor($event)}
        color-rgb={this.color}>
        <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
      </deckgo-color>,
    ];
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
