import {Component, Element, Event, EventEmitter, Method, Prop, State} from '@stencil/core';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoUtils} from '../../utils/deckdeckgo-utils';

enum DeckdeckgoSlideChartType {
  LINE = 'line',
  PIE = 'pie'
}

@Component({
  tag: 'deckgo-slide-chart',
  styleUrl: 'deckdeckgo-slide-chart.scss',
  shadow: true
})
export class DeckdeckgoSlideChart implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop() src: string;
  @Prop() separator: string = ';';

  @Prop() width: number;
  @Prop() height: number;

  @State() chartWidth: number;
  @State() chartHeight: number;

  @Prop() type: string = DeckdeckgoSlideChartType.PIE;

  // Pie
  @Prop() innerRadius: number = 0;
  @Prop() range: string[];

  async componentDidLoad() {
    await DeckdeckgoUtils.hideLazyLoadImages(this.el);

    this.initWindowResize();

    await this.initSize();

    this.slideDidLoad.emit();
  }

  @Method()
  beforeSwipe(_swipeLeft: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true)
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return DeckdeckgoSlideUtils.afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return DeckdeckgoSlideUtils.lazyLoadContent(this.el);
  }

  private initSize(): Promise<void> {
    return new Promise<void>((resolve) => {
      // If width and height, use them otherwise full size
      if (this.width > 0 && this.height > 0) {
        this.chartWidth = this.width;
        this.chartHeight = this.height;
      } else {
        const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-chart-container');

        if (container) {
          this.chartWidth = container.clientWidth;
          this.chartHeight = container.clientHeight;
        }
      }

      resolve();
    });
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', DeckdeckgoUtils.debounce(this.onResizeContent));
    }
  }

  private onResizeContent = async () => {
    await this.initSize();

    const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-pie-chart');

    if (element) {
      await (element as any).draw(this.chartWidth, this.chartHeight);
    }
  };

  render() {
    return <div class="deckgo-slide">
      <slot name="title"></slot>
      <div class="deckgo-chart-container">
        {this.renderChart()}
      </div>
    </div>
  }

  private renderChart() {
    if (this.type === DeckdeckgoSlideChartType.LINE) {
      return <deckgo-line-chart width={this.chartWidth} height={this.chartHeight} src={this.src} separator={this.separator}></deckgo-line-chart>
    } else {
      return <deckgo-pie-chart width={this.chartWidth} height={this.chartHeight} src={this.src} separator={this.separator}
                               inner-radius={this.innerRadius} ranger={this.range}></deckgo-pie-chart>
    }
  }

  hostData() {
    return {
      class: {
        'deckgo-slide-container': true
      }
    }
  }

}
