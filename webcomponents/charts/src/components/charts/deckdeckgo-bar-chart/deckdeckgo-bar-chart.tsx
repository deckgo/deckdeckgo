import {Component, Element, Method, Prop, Watch, h, Host, EventEmitter, Event} from '@stencil/core';

import {DeckdeckgoChart, DeckdeckgoChartUtils} from '../deckdeckgo-chart';

import {BaseType, Selection} from 'd3-selection';
import {scaleBand, scaleLinear} from 'd3-scale';
import {max} from 'd3-array';
import {Axis, axisBottom, axisLeft} from 'd3-axis';
import 'd3-transition';

import {DeckdeckgoBarChartData, DeckdeckgoBarChartDataValue} from '@deckdeckgo/types';

@Component({
  tag: 'deckgo-bar-chart',
  styleUrl: 'deckdeckgo-bar-chart.scss',
  shadow: true,
})
export class DeckdeckgoBarChart implements DeckdeckgoChart {
  @Element() el: HTMLElement;

  /**
   * The width of the chart
   */
  @Prop({mutable: true}) width: number;

  /**
   * The height of the chart
   */
  @Prop({mutable: true}) height: number;

  /**
   * The path to the source file of the data
   */
  @Prop() src: string;
  /**
   * The line separator use in your csv file
   */
  @Prop() separator: string = ';';

  /**
   * Set to `true` in case you would like to load (fetch) the data by yourself. Useful in case your data are protected with a token.
   */
  @Prop() customLoader: boolean = false;

  /**
   * The margin top of the chart in pixel
   */
  @Prop() marginTop: number = 32;
  /**
   * The margin bottom of the chart in pixel
   */
  @Prop() marginBottom: number = 64;
  /**
   * The margin left of the chart in pixel
   */
  @Prop() marginLeft: number = 32;
  /**
   * The margin right of the chart in pixel
   */
  @Prop() marginRight: number = 32;

  /**
   * Display multiple graphs and animate the transition between these
   */
  @Prop() animation: boolean = false;
  /**
   * Duration of the transition between graphs
   */
  @Prop() animationDuration: number = 1000;

  private svg: Selection<BaseType, any, HTMLElement, any>;
  private x0: any;
  private x1: any;
  private y: any;

  /**
   * Instead of a source file, source data can also be provided as an array of `DeckdeckgoBarChartData`
   */
  @Prop() data: DeckdeckgoBarChartData[];
  private chartData: DeckdeckgoBarChartData[];

  private barDataIndex: number = 0;

  private randomColors: string[];

  /**
   * If `false`, no axis y will be draw.
   */
  @Prop() yAxis: boolean = true;
  /**
   * Set a minimal value for the y Axis. Useful in case the series of data could contains only zeros.
   */
  @Prop() yAxisMin: number = 0;

  /**
   * The event to be processed to load the data if you are using a custom loader.
   * @private
   */
  @Event()
  chartCustomLoad: EventEmitter<string>;

  async componentDidLoad() {
    await this.draw();
  }

  @Watch('src')
  async onSrcChange() {
    await this.draw();
  }

  @Watch('data')
  async onDataChange() {
    await this.draw();
  }

  /**
   * This is the method we are using to refresh the current bar chart when an audience is participating to live vote. It will not redraw the axis but it will draw and animate the bars.
   * @param values
   */
  @Method()
  async updateCurrentBar(values: DeckdeckgoBarChartDataValue[]) {
    if (!this.x0 || !this.x1 || !this.y) {
      return;
    }

    if (!this.chartData || this.chartData.length <= 0) {
      return;
    }

    if (this.barDataIndex < 0 || this.barDataIndex >= this.chartData.length) {
      return;
    }

    this.chartData[this.barDataIndex].values = values;

    await this.drawBars(this.barDataIndex, this.animationDuration);
  }

  /**
   * In case you would like to redraw your chart, for example on resize of the window.
   * @param width
   * @param height
   */
  @Method()
  draw(width?: number, height?: number): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (width > 0) {
        this.width = width;
      }

      if (height > 0) {
        this.height = height;
      }

      if (!this.width || !this.height) {
        resolve();
        return;
      }

      this.svg = DeckdeckgoChartUtils.initSvg(this.el, this.width + this.marginLeft + this.marginRight, this.height + this.marginTop + this.marginBottom);
      this.svg = this.svg.append('g').attr('transform', 'translate(' + (this.marginLeft + this.marginRight) + ',' + (this.marginTop + this.marginBottom) + ')');

      this.barDataIndex = 0;
      this.chartData = this.data;

      if (this.src) {
        this.chartData = await this.fetchData();
      }

      if (!this.chartData || this.chartData === undefined || this.chartData.length <= 0) {
        resolve();
        return;
      }

      await this.firstDraw();

      resolve();
    });
  }

  private async firstDraw() {
    await this.initAxis();

    await this.drawAxis();

    this.randomColors = Array.from({length: this.chartData[0].values.length}, (_v, _i) => Math.floor(Math.random() * 16777215).toString(16));

    await this.drawBars(0, 0);
  }

  private async drawBars(index: number, animationDuration: number) {
    await this.initAxisXDomain();
    await this.initAxisYDomain();

    if (this.animation) {
      await this.drawAnimatedBars(index, animationDuration);
    } else {
      await this.drawInstantBars();
    }
  }

  private initAxisXDomain(): Promise<void> {
    return new Promise<void>((resolve) => {
      const xDomains = this.chartData[0].values.map((d) => {
        return d.label;
      });

      if (this.animation) {
        this.initAnimatedAxisX(xDomains);
      } else {
        this.initInstantAxisX(xDomains);
      }

      resolve();
    });
  }

  private initAxisYDomain(): Promise<void> {
    return new Promise<void>((resolve) => {
      const maxValue: number = max(this.chartData, (category) => {
        return max(category.values, (d) => {
          return d.value;
        });
      });

      this.y.domain([0, Math.max(this.yAxisMin, maxValue)]);

      resolve();
    });
  }

  /**
   * If you are using animation, this method is used to display the next data respectively the next chart.
   */
  @Method()
  async next() {
    await this.prevNext(true);
  }

  /**
   * If you are using animation, this method is used to display the previous data respectively the previous chart.
   */
  @Method()
  async prev() {
    await this.prevNext(false);
  }

  private async prevNext(next: boolean) {
    if (!this.animation) {
      return;
    }

    if (!this.chartData || this.chartData.length <= 0) {
      return;
    }

    if (next && this.barDataIndex + 1 < this.chartData.length) {
      this.barDataIndex++;
      await this.drawBars(this.barDataIndex, this.animationDuration);
    } else if (!next && this.barDataIndex > 0) {
      this.barDataIndex--;
      await this.drawBars(this.barDataIndex, this.animationDuration);
    }
  }

  /**
   * Is animation at the begin of the serie.
   */
  @Method()
  isBeginning(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!this.animation) {
        resolve(true);
        return;
      }

      resolve(this.barDataIndex === 0);
    });
  }

  /**
   * Is animation at the end of the serie.
   */
  @Method()
  isEnd(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!this.animation) {
        resolve(true);
        return;
      }

      resolve(this.barDataIndex === this.chartData.length - 1);
    });
  }

  private drawAxis(): Promise<void> {
    return new Promise<void>((resolve) => {
      const bottomAxis: Axis<any> = axisBottom(this.x0);

      this.svg
        .append('g')
        .attr('class', 'axis axis-x')
        .attr('transform', 'translate(0,' + (this.height - this.marginTop - this.marginBottom) + ')')
        .call(bottomAxis)
        .selectAll('text')
        .attr('transform', 'translate(-8,8)rotate(-45)')
        .style('text-anchor', 'end');

      if (!this.yAxis) {
        resolve();
        return;
      }

      const leftAxis: Axis<any> = axisLeft(this.y);

      this.svg.append('g').attr('class', 'axis axis-y').call(leftAxis);

      resolve();
    });
  }

  private initAxis(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.x0 = scaleBand().rangeRound([0, this.width - this.marginLeft - this.marginRight]);

      this.x1 = scaleBand().padding(0.05);
      this.y = scaleLinear().rangeRound([this.height - this.marginTop - this.marginBottom, 0]);

      await this.initAxisXDomain();
      await this.initAxisYDomain();

      resolve();
    });
  }

  private initAnimatedAxisX(xDomains: string[]) {
    this.x0.domain(xDomains).padding(0.05);
  }

  private initInstantAxisX(xDomains: string[]) {
    const categoriesNames = this.chartData.map((d) => {
      return d.label;
    });

    this.x0.paddingInner(0.1).domain(categoriesNames);
    this.x1.domain(xDomains).rangeRound([0, this.x0.bandwidth()]);
  }

  private drawAnimatedBars(index: number, animationDuration: number): Promise<void> {
    return new Promise<void>((resolve) => {
      const section: any = this.svg.selectAll('rect').data(this.chartData[index].values);

      section
        .enter()
        .append('rect')
        .merge(section)
        .attr('style', (d, i) => {
          return (
            'fill: var(--deckgo-chart-fill-color-' +
            d.key +
            ', ' +
            (this.randomColors && this.randomColors.length > i ? `#${this.randomColors[i]}` : '') +
            '); fill-opacity: var(--deckgo-chart-fill-opacity-' +
            d.key +
            '); stroke: var(--deckgo-chart-stroke-' +
            d.key +
            '); stroke-width: var(--deckgo-chart-stroke-width-' +
            d.key +
            ')'
          );
        })
        .transition()
        .duration(animationDuration)
        .attr('x', (d) => {
          return this.x0(d.label);
        })
        .attr('y', (d) => {
          return this.y(d.value);
        })
        .attr('width', this.x0.bandwidth())
        .attr('height', (d) => {
          const height: number = this.height - this.marginTop - this.marginBottom - this.y(d.value);
          return height >= 0 ? height : 0;
        });

      resolve();
    });
  }

  private drawInstantBars(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.svg
        .append('g')
        .selectAll('g')
        .data(this.chartData)
        .enter()
        .append('g')
        .attr('transform', (d) => {
          return 'translate(' + this.x0(d.label) + ',0)';
        })
        .selectAll('rect')
        .data((d) => {
          return d.values;
        })
        .enter()
        .append('rect')
        .attr('x', (d) => {
          return this.x1(d.label);
        })
        .attr('y', (d) => {
          return this.y(d.value);
        })
        .attr('width', this.x1.bandwidth())
        .attr('height', (d) => {
          const height: number = this.height - this.marginTop - this.marginBottom - this.y(d.value);
          return height >= 0 ? height : 0;
        })
        .attr('style', (d, i) => {
          return (
            'fill: var(--deckgo-chart-fill-color-' +
            d.key +
            ', ' +
            (this.randomColors && this.randomColors.length > i ? `#${this.randomColors[i]}` : '') +
            '); fill-opacity: var(--deckgo-chart-fill-opacity-' +
            d.key +
            '); stroke: var(--deckgo-chart-stroke-' +
            d.key +
            '); stroke-width: var(--deckgo-chart-stroke-width-' +
            d.key +
            ')'
          );
        });

      resolve();
    });
  }

  fetchData(): Promise<DeckdeckgoBarChartData[]> {
    return new Promise<DeckdeckgoBarChartData[]>(async (resolve) => {
      if (!this.src) {
        resolve([]);
        return;
      }

      if (this.customLoader) {
        this.chartCustomLoad.emit(this.src);
        resolve([]);
        return;
      }

      const response: Response = await fetch(this.src);
      const content: string = await response.text();

      let results: DeckdeckgoBarChartData[] = await this.loadContent(content);

      resolve(results);
    });
  }

  /**
   * If you "manually" load the data, call this method once the text content fetched.
   * @param content
   */
  @Method()
  async postCustomLoad(content: string | undefined) {
    this.chartData = await this.loadContent(content);

    if (!this.chartData || this.chartData === undefined || this.chartData.length <= 0) {
      return;
    }

    await this.firstDraw();
  }

  private loadContent(content: string | undefined): Promise<DeckdeckgoBarChartData[]> {
    return new Promise<DeckdeckgoBarChartData[]>(async (resolve) => {
      if (!content || content === undefined) {
        resolve([]);
        return;
      }

      const lines: string[] = content.split('\n');

      if (!lines || lines.length <= 0) {
        resolve([]);
        return;
      }

      let results: DeckdeckgoBarChartData[] = [];
      let keys: (number | string)[];

      lines.forEach((line: string, lineCount: number) => {
        const values: string[] = line.split(this.separator);

        if (values && values.length >= 2) {
          // Title
          if (!keys) {
            keys = Array.apply(null, {length: values.length}).map(Number.call, Number).slice(1);
          }

          let dataValues: DeckdeckgoBarChartDataValue[] = [];
          for (let i = 1; i < values.length; i++) {
            const tmp: number = parseInt(values[i]);

            if (!isNaN(tmp)) {
              dataValues.push({
                key: `${i}`,
                label: keys.length >= i ? `${keys[i - 1]}` : `${i}`,
                value: tmp,
              });
            } else if (lineCount === 0 && keys.length >= i) {
              keys[i - 1] = values[i];
            }
          }

          if (dataValues && dataValues.length > 0) {
            results.push({
              label: values[0],
              values: dataValues,
            });
          }
        }
      });

      resolve(results);
    });
  }

  render() {
    return (
      <Host style={{width: `${this.width}px`, height: `${this.height}px`}}>
        <svg></svg>
      </Host>
    );
  }
}
