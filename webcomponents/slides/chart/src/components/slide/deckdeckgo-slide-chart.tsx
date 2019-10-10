import {Component, Element, Event, EventEmitter, Method, Prop, State, h, Host} from '@stencil/core';

import {DeckdeckgoSlide, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';
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
export class DeckdeckgoSlideChart implements DeckdeckgoSlide {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({reflect: true}) src: string;
  @Prop() separator: string = ';';

  @Prop() width: number;
  @Prop() height: number;

  @State() chartWidth: number;
  @State() chartHeight: number;

  @Prop({reflect: true}) type: string = DeckdeckgoSlideChartType.PIE;

  // Pie
  @Prop({reflect: true}) innerRadius: number = 0;
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

  @Prop() animation: boolean = false;
  @Prop() animationDuration: number = 1000;

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.initWindowResize();

    await this.drawChart();

    this.slideDidLoad.emit();
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
    return lazyLoadContent(this.el);
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
      window.addEventListener('resize', debounce(this.onResizeContent));
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
    await this.drawChart();
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
    if (this.type === DeckdeckgoSlideChartType.LINE) {
      return <deckgo-line-chart width={this.chartWidth} height={this.chartHeight} src={this.src}
                                separator={this.separator}
                                date-pattern={this.datePattern} y-axis-domain={this.yAxisDomain}
                                margin-top={this.marginTop} margin-bottom={this.marginBottom}
                                margin-right={this.marginRight} margin-left={this.marginLeft}
                                smooth={this.smooth} area={this.area} ticks={this.ticks}
                                grid={this.grid}
                                animation={this.animation} animation-duration={this.animationDuration}></deckgo-line-chart>
    } else if (this.type === DeckdeckgoSlideChartType.BAR) {
      return <deckgo-bar-chart width={this.chartWidth} height={this.chartHeight} src={this.src} separator={this.separator}
                                margin-top={this.marginTop} margin-bottom={this.marginBottom}
                                margin-right={this.marginRight} margin-left={this.marginLeft}
                               animation={this.animation} animation-duration={this.animationDuration}></deckgo-bar-chart>
    } else {
      return <deckgo-pie-chart width={this.chartWidth} height={this.chartHeight} src={this.src} separator={this.separator}
                               inner-radius={this.innerRadius} range={this.range}
                               animation={this.animation} animation-duration={this.animationDuration}></deckgo-pie-chart>
    }
  }

}
