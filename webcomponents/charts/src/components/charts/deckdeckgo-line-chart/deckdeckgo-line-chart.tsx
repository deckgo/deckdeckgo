import {Component, Element, Method, Prop, Watch, h} from '@stencil/core';

import parse from 'date-fns/parse';
import isValid from 'date-fns/isValid';

import {BaseType, Selection} from 'd3-selection';
import {scaleLinear, scaleTime} from 'd3-scale';
import {extent, max, min} from 'd3-array';
import {Axis, axisBottom, axisLeft} from 'd3-axis';
import {Area, area, curveMonotoneX} from 'd3-shape';
import {transition} from 'd3-transition';
import {easeLinear} from 'd3-ease';

import {DeckdeckgoChart, DeckdeckgoChartUtils} from '../deckdeckgo-chart';

interface DeckdeckgoLineChartData {
  when: Date | number;
  value: number;
  compare?: number;
}

enum DeckdeckgoLineChartAxisDomain {
  EXTENT = 'extent',
  MAX = 'max'
}

@Component({
  tag: 'deckgo-line-chart',
  styleUrl: 'deckdeckgo-line-chart.scss',
  shadow: true
})
export class DeckdeckgoLineChart implements DeckdeckgoChart {

  @Element() el: HTMLElement;

  @Prop() width: number;
  @Prop() height: number;

  @Prop() src: string;
  @Prop() separator: string = ';';

  // All supported date format: https://date-fns.org/v2.0.0-alpha.26/docs/parse
  @Prop() datePattern: string = 'yyyy-MM-dd';

  @Prop() marginTop: number = 32;
  @Prop() marginBottom: number = 32;
  @Prop() marginLeft: number = 32;
  @Prop() marginRight: number = 32;

  @Prop() yAxisDomain: string = DeckdeckgoLineChartAxisDomain.MAX;

  @Prop() smooth: boolean = true;
  @Prop() area: boolean = true;
  @Prop() ticks: number;
  @Prop() grid: boolean = false;

  @Prop() animation: boolean = false;
  @Prop() animationDuration: number = 2000;

  private svg: Selection<BaseType, any, HTMLElement, any>;
  private x: any;
  private y: any;

  private data: DeckdeckgoLineChartData[];

  async componentDidLoad() {
    await this.draw();
  }

  @Watch('width')
  @Watch('height')
  @Watch('src')
  async redraw() {
    await this.draw();
  }

  @Method()
  draw(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.width || !this.height || !this.src) {
        resolve();
        return;
      }

      this.svg = DeckdeckgoChartUtils.initSvg(this.el, (this.width + this.marginLeft + this.marginRight), (this.height + this.marginTop + this.marginBottom));
      this.svg = this.svg.append('g').attr('transform', 'translate(' + this.marginLeft + ',' + this.marginTop + ')');

      this.data = await this.fetchData();

      const hasComparisonData: boolean = this.data && this.data.length > 0 && this.data[0].compare !== null && !isNaN(this.data[0].compare);
      const isXAxisNumber: boolean = this.data && this.data.length > 0 && typeof this.data[0].when === 'number';

      await this.initAxis(hasComparisonData, isXAxisNumber);

      await this.drawAxis();

      await this.drawLine(false);

      if (hasComparisonData && !this.animation) {
        await this.drawLine(true);
      }

      resolve();
    });
  }

  @Method()
  async next() {
    await this.prevNext(true);
  }

  @Method()
  async prev() {
    await this.prevNext(false);
  }

  private async prevNext(compare: boolean) {
    if (!this.animation) {
      return;
    }

    await this.drawLine(compare);
  }

  private initAxis(hasComparisonData: boolean, isXAxisNumber: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      this.x = isXAxisNumber ? scaleLinear().range([0, this.width]) : scaleTime().range([0, this.width]);
      this.y = scaleLinear().range([this.height, 0]);

      this.x.domain(extent(this.data, (d: DeckdeckgoLineChartData) => d.when));

      if (this.yAxisDomain === DeckdeckgoLineChartAxisDomain.EXTENT) {
        if (hasComparisonData) {
          this.y.domain([min(this.data, (d: DeckdeckgoLineChartData) => Math.min(d.value, d.compare)), max(this.data, (d: DeckdeckgoLineChartData) => Math.max(d.value, d.compare))]);
        } else {
          this.y.domain(extent(this.data, (d: DeckdeckgoLineChartData) => d.value));
        }
      } else {
        this.y.domain([0, max(this.data, (d: DeckdeckgoLineChartData) => d.compare ? Math.max(d.value, d.compare) : d.value)]);
      }

      resolve();
    });
  }

  private drawAxis(): Promise<void> {
    return new Promise<void>((resolve) => {
      const bottomAxis: Axis<any> = this.ticks > 0 ? axisBottom(this.x).ticks(this.ticks) : axisBottom(this.x);
      const leftAxis: Axis<any> = this.ticks > 0 ? axisLeft(this.y).ticks(this.ticks) : axisLeft(this.y);

      const styleClassAxisX: string = 'axis axis-x' + (this.grid ? ' axis-grid' : '');
      const styleClassAxisY: string = 'axis axis-y' + (this.grid ? ' axis-grid' : '');

      if (this.grid) {
        bottomAxis.tickSize(-this.height).tickFormat(null);
        leftAxis.tickSize(-this.width).tickFormat(null);
      }

      this.svg.append('g')
        .attr('class', styleClassAxisX)
        .attr('transform', 'translate(0,' + this.height + ')')
        .call(bottomAxis);

      this.svg.append('g')
        .attr('class', styleClassAxisY)
        .call(leftAxis);

      resolve();
    })
  }

  private drawLine(compare: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      let line: Area<DeckdeckgoLineChartData> = area<DeckdeckgoLineChartData>()
        .x((d: DeckdeckgoLineChartData) => this.x(d.when));

      if (this.area) {
        line.y0(this.height).y1((d: DeckdeckgoLineChartData) => compare ? this.y(d.compare) : this.y(d.value));
      } else {
        line.y((d: DeckdeckgoLineChartData) => compare ? this.y(d.compare) : this.y(d.value));
      }

      if (this.smooth) {
        line.curve(curveMonotoneX);
      }

      if (this.animation) {
        this.drawAnimatedLine(compare, line);
      } else {
        this.drawInstantLine(compare, line);
      }
      
      resolve();
    })
  }

  private drawInstantLine(compare: boolean, line: Area<DeckdeckgoLineChartData>) {
    this.svg.append('path')
      .datum(this.data)
      .attr('class', compare ? 'area-compare' : 'area')
      .attr('d', line);
  }

  private drawAnimatedLine(compare: boolean, line: Area<DeckdeckgoLineChartData>) {
    const t = transition();

    const section: any = this.svg.selectAll('.area')
      .data([this.data], (d: DeckdeckgoLineChartData) => { return compare ? this.y(d.compare) : this.y(d.value) });

    section
      .enter()
      .append('path').merge(section)
      .transition(t).duration(this.animationDuration).ease(easeLinear)
      .attr('class',`${compare ? 'area area-compare' : 'area'}`)
      .attr('d', line);
  }

  async fetchData(): Promise<DeckdeckgoLineChartData[]> {
    return new Promise<DeckdeckgoLineChartData[]>(async (resolve) => {
      if (!this.src) {
        resolve([]);
        return;
      }

      const response: Response = await fetch(this.src);
      const content: string = await response.text();

      if (!content) {
        resolve([]);
        return;
      }

      const lines: string[] = content.split('\n');

      if (!lines || lines.length <= 0) {
        resolve([]);
        return;
      }

      let results: DeckdeckgoLineChartData[] = [];
      lines.forEach((line: string) => {
        const values: string[] = line.split(this.separator);

        if (values && values.length >= 1) {

          const when: any = this.validDate(values[0]) ? parse(values[0], this.datePattern, new Date()) : parseInt(values[0]);

          // Source file could contains one or two columns
          let data: DeckdeckgoLineChartData = {
            when: when,
            value: parseInt(values[1])
          };

          if (values.length >= 2) {
            data.compare = parseInt(values[2])
          }

          if (!isNaN(data.value) && data.when) {
            results.push(data);
          }
        }
      });

      resolve(results);
    });
  }

  private validDate(value: string): boolean {
    return this.datePattern && isValid(parse(value, this.datePattern, new Date()));
  }

  render() {
    return <svg></svg>;
  }

}
