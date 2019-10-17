import {Component, Element, Event, EventEmitter, Method, Prop, State, h, Host, Watch} from '@stencil/core';

import {DeckdeckgoSlideResize, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';
import {debounce} from '@deckdeckgo/utils';

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
export class DeckdeckgoSlideChart implements DeckdeckgoSlideResize {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({reflect: true, mutable: true}) src: string;
  @Prop() separator: string;

  @Prop() width: number;
  @Prop() height: number;

  @State() chartWidth: number;
  @State() chartHeight: number;

  @Prop({reflect: true}) type: string = DeckdeckgoSlideChartType.PIE;

  // Pie
  @Prop({reflect: true}) innerRadius: number = 0;
  @Prop() range: string[];

  // Line
  @Prop({reflect: true}) datePattern: string;

  @Prop() marginTop: number = 32;
  @Prop() marginBottom: number = 32;
  @Prop() marginLeft: number = 32;
  @Prop() marginRight: number = 32;

  @Prop({reflect: true}) yAxisDomain: string;

  @Prop({reflect: true}) smooth: string;
  @Prop({reflect: true}) area: string;
  @Prop({reflect: true}) ticks: number;
  @Prop({reflect: true}) grid: string;

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  @Prop({reflect: true}) animation: boolean = false;
  @Prop() animationDuration: number = 1000;

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.initWindowResize();

    this.slideDidLoad.emit();
  }

  @Watch('innerRadius')
  @Watch('datePattern')
  @Watch('yAxisDomain')
  @Watch('smooth')
  @Watch('area')
  @Watch('ticks')
  @Watch('grid')
  @Watch('animation')
  async onPropChanges() {
    await this.drawChart();
  }

  @Method()
  beforeSwipe(enter: boolean, _reveal: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      if (!this.animation) {
        resolve(true);
        return;
      }

      const chart: HTMLElement = this.el.shadowRoot.querySelector(this.type === DeckdeckgoSlideChartType.LINE ? 'deckgo-line-chart' : (this.type === DeckdeckgoSlideChartType.BAR ? 'deckgo-bar-chart' : 'deckgo-pie-chart'));

      if (!chart) {
        resolve(true);
        return;
      }

      const couldSwipe: boolean = enter ? await (chart as any).isEnd() : await (chart as any).isBeginning();

      if (couldSwipe) {
        resolve(true);
        return;
      }

      if (enter) {
        await (chart as any).next();
      } else {
        await (chart as any).prev();
      }

      resolve(false);
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const promises = [];
      promises.push(lazyLoadContent(this.el));
      promises.push(this.drawChart());

      await Promise.all(promises);

      resolve();
    });
  }

  @Method()
  revealContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  hideContent(): Promise<void> {
    return Promise.resolve();
  }

  private initSize(): Promise<void> {
    return new Promise<void>((resolve) => {
      // If width and height, use them otherwise full size
      if (this.width > 0 && this.height > 0) {
        this.chartWidth = this.width - this.marginLeft - this.marginRight;
        this.chartHeight = this.height - this.marginTop - this.marginBottom;
      } else {
        const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-chart-container');

        const width: number = container.clientWidth;
        const height: number = container.clientHeight;

        if (container && width > 0 && height > 0) {
          this.chartWidth = width - this.marginLeft - this.marginRight;
          this.chartHeight = height - this.marginTop - this.marginBottom;
        }
      }

      resolve();
    });
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onResizeContent, 500));
    }
  }

  private onResizeContent = async () => {
    await this.drawChart();
  };

  private async drawChart() {
    await this.initSize();

    const element: HTMLElement = this.el.shadowRoot.querySelector(this.type === DeckdeckgoSlideChartType.LINE ? 'deckgo-line-chart' : (this.type === DeckdeckgoSlideChartType.BAR ? 'deckgo-bar-chart' : 'deckgo-pie-chart'));

    if (element) {
      await (element as any).draw(this.chartWidth, this.chartHeight);
    }
  }

  @Method()
  async draw() {
    await this.onResizeContent();
  }

  @Method()
  resizeContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.onResizeContent();

      resolve();
    });
  }

  render() {
    return <Host class={{'deckgo-slide-container': true}}>
      <div class="deckgo-slide">
        <slot name="title"></slot>
        <div class="deckgo-chart-container">
          {this.renderChart()}
        </div>
        <slot name="notes"></slot>
        <slot name="actions"></slot>
        <slot name="background"></slot>
      </div>
    </Host>
  }

  private renderChart() {
    const attrs = {};

    if (this.separator) {
      attrs['separator'] = this.separator;
    }

    if (this.type === DeckdeckgoSlideChartType.LINE) {
      return this.renderLineChart(attrs);
    } else if (this.type === DeckdeckgoSlideChartType.BAR) {
      return <deckgo-bar-chart width={this.chartWidth} height={this.chartHeight} src={this.src}
                                margin-top={this.marginTop} margin-bottom={this.marginBottom}
                                margin-right={this.marginRight} margin-left={this.marginLeft}
                               animation={this.animation} animation-duration={this.animationDuration} {...attrs}></deckgo-bar-chart>
    } else {
      return <deckgo-pie-chart width={this.chartWidth} height={this.chartHeight} src={this.src}
                               inner-radius={this.innerRadius} range={this.range}
                               animation={this.animation} animation-duration={this.animationDuration} {...attrs}></deckgo-pie-chart>
    }
  }

  private renderLineChart(attrs) {
    if (this.datePattern) {
      attrs['date-pattern'] = this.datePattern;
    }

    if (this.yAxisDomain) {
      attrs['y-axis-domain'] = this.yAxisDomain;
    }

    attrs['smooth'] = this.smooth === 'false' ? false : true;
    attrs['area'] = this.area === 'false' ? false : true;
    attrs['grid'] = this.grid === 'true';

    if (this.ticks > 0) {
      attrs['ticks'] = this.ticks;
    }

    return <deckgo-line-chart width={this.chartWidth} height={this.chartHeight} src={this.src}
                              margin-top={this.marginTop} margin-bottom={this.marginBottom}
                              margin-right={this.marginRight} margin-left={this.marginLeft}
                              animation={this.animation} animation-duration={this.animationDuration}
                              {...attrs}></deckgo-line-chart>;
  }
}
