import {Component, Element, Event, EventEmitter, h, Prop, State, Host} from '@stencil/core';

import {SlideAttributesYAxisDomain, SlideChartType} from '../../../../models/data/slide';

import {EditAction} from '../../../../types/editor/edit-action';
import {ChartUtils} from '../../../../utils/editor/chart.utils';

@Component({
  tag: 'app-edit-slide-chart',
})
export class AppEditSlideChart {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  slideDidChange: EventEmitter<HTMLElement>;

  @Event()
  private action: EventEmitter<EditAction>;

  @State()
  private chartType: SlideChartType = undefined;

  @State()
  private datePattern: string = undefined;

  @State()
  private separator: string = undefined;

  @State()
  private smooth: boolean = true;

  @State()
  private area: boolean = true;

  @State()
  private grid: boolean = false;

  @State()
  private yAxisDomain: SlideAttributesYAxisDomain = 'max';

  @State()
  private ticks: string;

  @State()
  private innerRadius: string;

  async componentWillLoad() {
    this.chartType = await ChartUtils.initSlideChartType(this.selectedElement);

    this.datePattern = this.selectedElement ? this.selectedElement.getAttribute('date-pattern') : undefined;
    this.yAxisDomain =
      this.selectedElement && this.selectedElement.hasAttribute('y-axis-domain')
        ? (this.selectedElement.getAttribute('y-axis-domain') as SlideAttributesYAxisDomain)
        : 'max';

    this.smooth = this.selectedElement ? (this.selectedElement.getAttribute('smooth') === 'false' ? false : true) : true;
    this.area = this.selectedElement ? (this.selectedElement.getAttribute('area') === 'false' ? false : true) : true;
    this.grid = this.selectedElement ? (this.selectedElement.getAttribute('grid') === 'true' ? true : false) : false;

    this.ticks = this.selectedElement ? this.selectedElement.getAttribute('ticks') : undefined;

    this.separator = this.selectedElement ? this.selectedElement.getAttribute('separator') : undefined;

    this.innerRadius = this.selectedElement ? this.selectedElement.getAttribute('inner-radius') : undefined;
  }

  private handleDatePatternInput($event: CustomEvent<KeyboardEvent>) {
    this.datePattern = ($event.target as InputTargetEvent).value;
  }

  private handleTicksInput($event: CustomEvent<KeyboardEvent>) {
    this.ticks = ($event.target as InputTargetEvent).value;
  }

  private handleSeparatorInput($event: CustomEvent<KeyboardEvent>) {
    this.separator = ($event.target as InputTargetEvent).value;
  }

  private handleInnerRadiusInput($event: CustomEvent<KeyboardEvent>) {
    this.innerRadius = ($event.target as InputTargetEvent).value;
  }

  private applyChartChanges(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (this.datePattern && this.datePattern !== 'yyyy-MM-dd') {
        this.selectedElement.setAttribute('date-pattern', this.datePattern);
      } else {
        this.selectedElement.removeAttribute('date-pattern');
      }

      if (this.yAxisDomain && this.yAxisDomain !== 'max') {
        this.selectedElement.setAttribute('y-axis-domain', this.yAxisDomain);
      } else {
        this.selectedElement.removeAttribute('y-axis-domain');
      }

      this.selectedElement.setAttribute('smooth', `${this.smooth}`);
      this.selectedElement.setAttribute('area', `${this.area}`);

      this.selectedElement.setAttribute('grid', `${this.grid}`);

      if (this.ticks && !isNaN(this.ticks as any)) {
        this.selectedElement.setAttribute('ticks', this.ticks);
      } else {
        this.selectedElement.removeAttribute('ticks');
      }

      if (this.separator && this.separator !== '' && this.separator !== ';') {
        this.selectedElement.setAttribute('separator', this.separator);
      } else {
        this.selectedElement.removeAttribute('separator');
      }

      if (this.innerRadius && !isNaN(this.innerRadius as any)) {
        this.selectedElement.setAttribute('inner-radius', this.innerRadius);
      } else {
        this.selectedElement.removeAttribute('inner-radius');
      }

      this.slideDidChange.emit(this.selectedElement);

      resolve();
    });
  }

  private toggleYAxisDomain($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      this.yAxisDomain = $event.detail.value;

      await this.applyChartChanges();

      resolve();
    });
  }

  private async toggleSmooth() {
    this.smooth = !this.smooth;

    await this.applyChartChanges();
  }

  private async toggleArea() {
    this.area = !this.area;

    await this.applyChartChanges();
  }

  private async toggleGrid() {
    this.grid = !this.grid;

    await this.applyChartChanges();
  }

  render() {
    return (
      <Host>
        {this.renderChartLineOptions()}
        {this.renderChartPieOptions()}

        {this.renderChartDataOptions()}

        <ion-item-divider>
          <ion-label>Data separator</ion-label>
        </ion-item-divider>

        <ion-item class="with-padding">
          <ion-input
            value={this.separator}
            placeholder=";"
            debounce={500}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleSeparatorInput(e)}
            onIonChange={() => this.applyChartChanges()}></ion-input>
        </ion-item>

        <ion-item class="action-button ion-margin-top">
          <ion-button shape="round" onClick={() => this.action.emit(EditAction.OPEN_DATA)} color="tertiary">
            <ion-label>Modify data source</ion-label>
          </ion-button>
        </ion-item>
      </Host>
    );
  }

  private renderChartDataOptions() {
    if (this.chartType !== SlideChartType.LINE) {
      return undefined;
    }

    return [
      <ion-item-divider class="ion-margin-top">
        <h4 class="ion-no-margin">Data source</h4>
      </ion-item-divider>,

      <ion-item-divider>
        <ion-label>Date pattern</ion-label>
      </ion-item-divider>,

      <ion-item class="with-padding">
        <ion-input
          value={this.datePattern}
          placeholder="yyyy-MM-dd"
          debounce={500}
          onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleDatePatternInput(e)}
          onIonChange={() => this.applyChartChanges()}></ion-input>
      </ion-item>,
    ];
  }

  private renderChartPieOptions() {
    if (this.chartType !== SlideChartType.PIE) {
      return undefined;
    }

    return [
      <ion-item-divider>Inner radius</ion-item-divider>,

      <ion-item class="with-padding">
        <ion-input
          value={this.innerRadius}
          type="number"
          placeholder="A radius to display a donut"
          debounce={500}
          onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInnerRadiusInput(e)}
          onIonChange={() => this.applyChartChanges()}></ion-input>
      </ion-item>,
    ];
  }

  private renderChartLineOptions() {
    if (this.chartType !== SlideChartType.LINE) {
      return undefined;
    }

    return [
      <ion-item>
        <ion-label>Smooth</ion-label>
        <ion-checkbox slot="end" checked={this.smooth} onIonChange={() => this.toggleSmooth()}></ion-checkbox>
      </ion-item>,

      <ion-item>
        <ion-label>Area</ion-label>
        <ion-checkbox slot="end" checked={this.area} onIonChange={() => this.toggleArea()}></ion-checkbox>
      </ion-item>,

      <ion-item>
        <ion-label>Grid</ion-label>
        <ion-checkbox slot="end" checked={this.grid} onIonChange={() => this.toggleGrid()}></ion-checkbox>
      </ion-item>,

      <ion-item-divider class="ion-margin-top">Y-axis</ion-item-divider>,

      <ion-item class="select">
        <ion-label>Domain</ion-label>

        <ion-select
          value={this.yAxisDomain}
          placeholder="Domain"
          onIonChange={(e: CustomEvent) => this.toggleYAxisDomain(e)}
          interface="popover"
          mode="md"
          class="ion-padding-start ion-padding-end">
          <ion-select-option value="max">Max</ion-select-option>
          <ion-select-option value="extent">Extent</ion-select-option>
        </ion-select>
      </ion-item>,

      <ion-item-divider>Ticks</ion-item-divider>,

      <ion-item class="with-padding">
        <ion-input
          value={this.ticks}
          type="number"
          placeholder="A custom number of ticks"
          debounce={500}
          onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleTicksInput(e)}
          onIonChange={() => this.applyChartChanges()}></ion-input>
      </ion-item>,
    ];
  }
}
