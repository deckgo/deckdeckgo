import {Component, Element, Event, EventEmitter, Method, Prop, State} from '@stencil/core';

import {DeckDeckGoUtils} from '@deckdeckgo/utils';

import {DeckdeckgoSlide, DeckdeckgoSlideUtils} from '../deckdeckgo-slide';
import {DeckdeckgoDeckUtils} from '../../utils/deckdeckgo-deck-utils';

enum DeckdeckgoSlideChartType {
  LINE = 'line',
  PIE = 'pie',
  BAR = 'bar'
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

  // Line
  @Prop() datePattern: string = 'yyyy-MM-dd';

  @Prop() marginTop: number = 32;
  @Prop() marginBottom: number = 32;
  @Prop() marginLeft: number = 32;
  @Prop() marginRight: number = 32;

  @Prop() yAxisDomain: string = 'max';

  @Prop() smooth: boolean = true;
  @Prop() area: boolean = true;
  @Prop() ticks: number;
  @Prop() grid: boolean = false;

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  async componentDidLoad() {
    await DeckdeckgoDeckUtils.hideLazyLoadImages(this.el);

    this.initWindowResize();

    await this.initSize();

    this.slideDidLoad.emit();
  }

  @Method()
  beforeSwipe(_enter: boolean): Promise<boolean> {
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
        this.chartWidth = this.width - this.marginLeft - this.marginRight;
        this.chartHeight = this.height - this.marginTop - this.marginBottom;
      } else {
        const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-chart-container');

        if (container) {
          this.chartWidth = container.clientWidth - this.marginLeft - this.marginRight;
          this.chartHeight = container.clientHeight - this.marginTop - this.marginBottom;
        }
      }

      resolve();
    });
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', DeckDeckGoUtils.debounce(this.onResizeContent));
    }
  }

  private onResizeContent = async () => {
    await this.initSize();

    const element: HTMLElement = this.el.shadowRoot.querySelector(this.type === DeckdeckgoSlideChartType.LINE ? 'deckgo-line-chart' : 'deckgo-pie-chart');

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
      <slot name="notes"></slot>
      <slot name="actions"></slot>
      <slot name="background"></slot>
    </div>
  }

  private renderChart() {
    if (this.type === DeckdeckgoSlideChartType.LINE) {
      return <deckgo-line-chart width={this.chartWidth} height={this.chartHeight} src={this.src}
                                separator={this.separator}
                                date-pattern={this.datePattern} y-axis-domain={this.yAxisDomain}
                                margin-top={this.marginTop} margin-bottom={this.marginBottom}
                                margin-right={this.marginRight} margin-left={this.marginLeft}
                                smooth={this.smooth} area={this.area} ticks={this.ticks}
                                grid={this.grid}></deckgo-line-chart>
    } else if (this.type === DeckdeckgoSlideChartType.BAR) {
      return <deckgo-bar-chart width={this.chartWidth} height={this.chartHeight} src={this.src} separator={this.separator}
                                margin-top={this.marginTop} margin-bottom={this.marginBottom}
                                margin-right={this.marginRight} margin-left={this.marginLeft}></deckgo-bar-chart>
    } else {
      return <deckgo-pie-chart width={this.chartWidth} height={this.chartHeight} src={this.src} separator={this.separator}
                               inner-radius={this.innerRadius} range={this.range}></deckgo-pie-chart>
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
