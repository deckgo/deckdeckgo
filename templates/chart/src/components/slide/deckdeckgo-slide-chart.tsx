import {Component, Element, Event, EventEmitter, Method, Prop, State, h, Host, Watch} from '@stencil/core';

import {DeckdeckgoSlideResize, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';
import {debounce} from '@deckdeckgo/utils';

enum DeckdeckgoSlideChartType {
  LINE = 'line',
  PIE = 'pie',
  BAR = 'bar'
}

/**
 * Manually define the list of methods of the charts as we do not want to explicitly add these to the bundle.
 */
interface HTMLDeckgoChartElement extends HTMLElement {
  postCustomLoad(content: string | undefined): Promise<void>;
  getRandomColors(): Promise<string[]>;
  draw(width?: number, height?: number): Promise<void>;
  next(): Promise<void>;
  prev(): Promise<void>;
  isBeginning(): Promise<boolean>;
  isEnd(): Promise<boolean>;
}

/**
 * @slot title - A title
 * @slot notes - Some notes related to this slide
 * @slot actions - Custom actions for this slide
 * @slot background - A custom background for this slide
 * @slot header - A custom header for this slide
 * @slot footer - A custom footer for this slide
 */
@Component({
  tag: 'deckgo-slide-chart',
  styleUrl: 'deckdeckgo-slide-chart.scss',
  shadow: true
})
export class DeckdeckgoSlideChart implements DeckdeckgoSlideResize {
  @Element() el: HTMLElement;

  /**
   * Triggered when the slide is loaded
   */
  @Event()
  slideDidLoad: EventEmitter<void>;

  @Prop({reflect: true, mutable: true}) src: string;
  @Prop({reflect: true}) separator: string;

  @Prop() customLoader: boolean = false;

  @Prop() width: number;
  @Prop() height: number;

  @State() chartWidth: number;
  @State() chartHeight: number;

  /**
   * The type of the chart, pie, line or bar
   */
  @Prop({reflect: true}) type: string = DeckdeckgoSlideChartType.PIE;

  // Pie
  @Prop({reflect: true}) innerRadius: number;
  @Prop() range: string[];

  // Line
  @Prop({reflect: true}) datePattern: string;

  @Prop() marginTop: number = 8;
  @Prop() marginBottom: number = 64;
  @Prop() marginLeft: number = 32;
  @Prop() marginRight: number = 32;

  @Prop({reflect: true}) yAxisDomain: string;

  @Prop({reflect: true}) smooth: string;
  @Prop({reflect: true}) area: string;
  @Prop({reflect: true}) ticks: number;
  @Prop({reflect: true}) grid: string;

  @Prop({reflect: true}) animation: boolean = false;
  @Prop() animationDuration: number = 1000;

  private redrawAfterUpdate: boolean = false;

  /**
   * If you define a background for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customBackground: boolean = false;

  /**
   * If you provide actions for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customActions: boolean = false;

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
  @Watch('separator')
  async onPropChanges() {
    this.redrawAfterUpdate = true;
  }

  async componentDidUpdate() {
    if (this.redrawAfterUpdate) {
      await this.drawChart();
      this.redrawAfterUpdate = false;
    }
  }

  @Method()
  beforeSwipe(enter: boolean, _reveal: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      if (!this.animation) {
        resolve(true);
        return;
      }

      const chart: HTMLDeckgoChartElement = this.el.shadowRoot.querySelector(
        this.type === DeckdeckgoSlideChartType.LINE
          ? 'deckgo-line-chart'
          : this.type === DeckdeckgoSlideChartType.BAR
          ? 'deckgo-bar-chart'
          : 'deckgo-pie-chart'
      );

      if (!chart) {
        resolve(true);
        return;
      }

      const couldSwipe: boolean = enter ? await chart.isEnd() : await chart.isBeginning();

      if (couldSwipe) {
        resolve(true);
        return;
      }

      if (enter) {
        await chart.next();
      } else {
        await chart.prev();
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
        this.chartWidth = this.width - (this.type !== DeckdeckgoSlideChartType.PIE ? this.marginLeft + this.marginRight : 0);
        this.chartHeight = this.height - (this.type !== DeckdeckgoSlideChartType.PIE ? this.marginTop + this.marginBottom : 0);
      } else {
        const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-chart-container');

        const width: number = container.clientWidth;
        const height: number = container.clientHeight;

        if (container && width > 0 && height > 0) {
          this.chartWidth = width - (this.type !== DeckdeckgoSlideChartType.PIE ? this.marginLeft + this.marginRight : 0);
          this.chartHeight = height - (this.type !== DeckdeckgoSlideChartType.PIE ? this.marginTop + this.marginBottom : 0);
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

    const element: HTMLDeckgoChartElement = this.el.shadowRoot.querySelector(
      this.type === DeckdeckgoSlideChartType.LINE
        ? 'deckgo-line-chart'
        : this.type === DeckdeckgoSlideChartType.BAR
        ? 'deckgo-bar-chart'
        : 'deckgo-pie-chart'
    );

    await element?.draw(this.chartWidth, this.chartHeight);
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

  @Method()
  async postCustomLoad(content: string | undefined) {
    const chartElement: HTMLDeckgoChartElement = this.getChartElement();
    await chartElement?.postCustomLoad(content);
  }

  /**
   * Returns the list of the random colors that have been generated.
   */
  @Method()
  async getRandomColors(): Promise<string[] | undefined | null> {
    const chartElement: HTMLDeckgoChartElement = this.getChartElement();
    return chartElement?.getRandomColors();
  }

  private getChartElement(): HTMLDeckgoChartElement | null {
    if (this.type === DeckdeckgoSlideChartType.LINE) {
      return this.el.shadowRoot.querySelector('deckgo-line-chart') as HTMLDeckgoChartElement | null;
    } else if (this.type === DeckdeckgoSlideChartType.BAR) {
      return this.el.shadowRoot.querySelector('deckgo-bar-chart') as HTMLDeckgoChartElement | null;
    } else {
      return this.el.shadowRoot.querySelector('deckgo-pie-chart') as HTMLDeckgoChartElement | null;
    }
  }

  private onChartRandomColor($event: CustomEvent<string[]>) {
    $event.detail.forEach((color: string, i: number) => {
      const key: string = `--deckgo-chart-fill-color-${i + 1}`;

      const css: CSSStyleDeclaration = window.getComputedStyle(this.el);

      // Do not overwrite if style already set on the host
      const style: string | undefined = css.getPropertyValue(key);
      if (style === undefined || style === '') {
        this.el.style.setProperty(key, `#${color}`);
      }
    });
  }

  render() {
    return (
      <Host class={{'deckgo-slide-container': true}}>
        <div class="deckgo-slide">
          <slot name="title"></slot>
          <div class="deckgo-chart-container">{this.renderChart()}</div>
          <slot name="notes"></slot>
          <slot name="actions"></slot>
          <slot name="background"></slot>
          <slot name="header"></slot>
          <slot name="footer"></slot>
        </div>
      </Host>
    );
  }

  private renderChart() {
    const attrs = {
      separator: this.separator ? this.separator : ';'
    };

    if (this.type === DeckdeckgoSlideChartType.LINE) {
      return this.renderLineChart(attrs);
    } else if (this.type === DeckdeckgoSlideChartType.BAR) {
      return (
        <deckgo-bar-chart
          width={this.chartWidth}
          height={this.chartHeight}
          src={this.src}
          custom-loader={this.customLoader}
          margin-top={this.marginTop}
          margin-bottom={this.marginBottom}
          margin-right={this.marginRight}
          margin-left={this.marginLeft}
          animation={this.animation}
          animation-duration={this.animationDuration}
          onChartRandomColor={($event: CustomEvent<string[]>) => this.onChartRandomColor($event)}
          {...attrs}></deckgo-bar-chart>
      );
    } else {
      return this.renderPieChart(attrs);
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

    return (
      <deckgo-line-chart
        width={this.chartWidth}
        height={this.chartHeight}
        src={this.src}
        custom-loader={this.customLoader}
        margin-top={this.marginTop}
        margin-bottom={this.marginBottom}
        margin-right={this.marginRight}
        margin-left={this.marginLeft}
        animation={this.animation}
        animation-duration={this.animationDuration}
        onChartRandomColor={($event: CustomEvent<string[]>) => this.onChartRandomColor($event)}
        {...attrs}></deckgo-line-chart>
    );
  }

  private renderPieChart(attrs) {
    if (this.innerRadius > 0) {
      attrs['inner-radius'] = this.innerRadius;
    }

    return (
      <deckgo-pie-chart
        width={this.chartWidth}
        height={this.chartHeight}
        src={this.src}
        custom-loader={this.customLoader}
        margin-top={this.marginTop}
        margin-bottom={this.marginBottom}
        margin-right={this.marginRight}
        margin-left={this.marginLeft}
        animation={this.animation}
        animation-duration={this.animationDuration}
        onChartRandomColor={($event: CustomEvent<string[]>) => this.onChartRandomColor($event)}
        {...attrs}></deckgo-pie-chart>
    );
  }
}
