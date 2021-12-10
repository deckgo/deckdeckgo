import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {SlideChartType} from '@deckdeckgo/editor';
import {hexToRgb} from '@deckdeckgo/utils';

import i18n from '../../../../../../stores/i18n.store';

import {ColorUtils, InitStyleColor} from '../../../../../../utils/editor/color.utils';
import {ChartUtils} from '../../../../../../utils/editor/chart.utils';

enum ApplyColorType {
  FILL,
  STROKE,
  FONT,
  AXIS,
  GRID
}

@Component({
  tag: 'app-color-chart'
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
        opacity: null
      };
    }

    if (this.applyColorType === ApplyColorType.FILL) {
      return ColorUtils.splitColor(
        this.hexOrRgb(this.selectedElement.style.getPropertyValue(`--deckgo-chart-fill-color-${this.colorIndex}`))
      );
    } else if (this.applyColorType === ApplyColorType.STROKE) {
      return ColorUtils.splitColor(this.hexOrRgb(this.selectedElement.style.getPropertyValue(`--deckgo-chart-stroke-${this.colorIndex}`)));
    } else if (this.applyColorType === ApplyColorType.AXIS) {
      return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--deckgo-chart-axis-color'));
    } else if (this.applyColorType === ApplyColorType.GRID) {
      return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--deckgo-chart-grid-stroke'));
    } else {
      return ColorUtils.splitColor(this.selectedElement.style.getPropertyValue('--deckgo-chart-text-color'));
    }
  };

  private hexOrRgb(color: string) {
    const hexColor: string | undefined = hexToRgb(color);
    return hexColor ? hexColor : color;
  }

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
              onIonChange={(e: CustomEvent) => this.toggleColorType(e)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              {this.renderColorOptions()}
            </ion-select>
          </ion-item>

          <ion-item-divider class="ion-padding-top">
            <ion-label>{i18n.state.editor.series}</ion-label>
          </ion-item-divider>

          <ion-item class="select">
            <ion-label>{i18n.state.editor.series}</ion-label>

            <ion-select
              value={this.colorIndex}
              placeholder={i18n.state.editor.series_index}
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
          <ion-label>{i18n.state.editor.color}</ion-label>
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
      <ion-select-option value={ApplyColorType.FILL}>{i18n.state.editor.fill}</ion-select-option>,
      <ion-select-option value={ApplyColorType.STROKE}>{i18n.state.editor.stroke}</ion-select-option>,
      <ion-select-option value={ApplyColorType.FONT}>{i18n.state.editor.font}</ion-select-option>
    ];

    if (this.chartType != SlideChartType.PIE) {
      options.push(<ion-select-option value={ApplyColorType.AXIS}>{i18n.state.editor.axis}</ion-select-option>);
      options.push(<ion-select-option value={ApplyColorType.GRID}>{i18n.state.editor.grid}</ion-select-option>);
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
