import {Component, Element, Method, Prop, Watch, h} from '@stencil/core';

import parse from 'date-fns/parse';
import isValid from 'date-fns/isValid';

import {BaseType, Selection} from 'd3-selection';
import {scaleLinear, scaleTime} from 'd3-scale';
import {extent, max, min} from 'd3-array';
import {Axis, axisBottom, axisLeft} from 'd3-axis';
import {Area, area, curveMonotoneX} from 'd3-shape';

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

      let svg: Selection<BaseType, any, HTMLElement, any> = DeckdeckgoChartUtils.initSvg(this.el, (this.width + this.marginLeft + this.marginRight), (this.height + this.marginTop + this.marginBottom));
      svg = svg.append('g').attr('transform', 'translate(' + this.marginLeft + ',' + this.marginTop + ')');

      const data: DeckdeckgoLineChartData[] = await this.fetchData();

      const hasComparisonData: boolean = data && data.length > 0 && data[0].compare !== null && !isNaN(data[0].compare);
      const isXAxisNumber: boolean = data && data.length > 0 && typeof data[0].when === 'number';

      const {x, y} = await this.initAxis(data, hasComparisonData, isXAxisNumber);

      await this.drawAxis(svg, x, y);

      await this.drawLine(svg, x, y, data, false);

      if (hasComparisonData) {
        await this.drawLine(svg, x, y, data, true);
      }

      resolve();
    });
  }

  private initAxis(data: DeckdeckgoLineChartData[], hasComparisonData: boolean, isXAxisNumber: boolean): Promise<any> {
    return new Promise<any>((resolve) => {
      let x: any = isXAxisNumber ? scaleLinear().range([0, this.width]) : scaleTime().range([0, this.width]);
      let y: any = scaleLinear().range([this.height, 0]);

      x.domain(extent(data, (d: DeckdeckgoLineChartData) => d.when));

      if (this.yAxisDomain === DeckdeckgoLineChartAxisDomain.EXTENT) {
        if (hasComparisonData) {
          y.domain([min(data, (d: DeckdeckgoLineChartData) => Math.min(d.value, d.compare)), max(data, (d: DeckdeckgoLineChartData) => Math.max(d.value, d.compare))]);
        } else {
          y.domain(extent(data, (d: DeckdeckgoLineChartData) => d.value));
        }
      } else {
        y.domain([0, max(data, (d: DeckdeckgoLineChartData) => d.compare ? Math.max(d.value, d.compare) : d.value)]);
      }

      resolve({x, y});
    });
  }

  private drawAxis(svg: Selection<BaseType, any, HTMLElement, any>, x: any, y: any): Promise<void> {
    return new Promise<void>((resolve) => {
      const bottomAxis: Axis<any> = this.ticks > 0 ? axisBottom(x).ticks(this.ticks) : axisBottom(x);
      const leftAxis: Axis<any> = this.ticks > 0 ? axisLeft(y).ticks(this.ticks) : axisLeft(y);

      const styleClassAxisX: string = 'axis axis-x' + (this.grid ? ' axis-grid' : '');
      const styleClassAxisY: string = 'axis axis-y' + (this.grid ? ' axis-grid' : '');

      if (this.grid) {
        bottomAxis.tickSize(-this.height).tickFormat(null);
        leftAxis.tickSize(-this.width).tickFormat(null);
      }

      svg.append('g')
        .attr('class', styleClassAxisX)
        .attr('transform', 'translate(0,' + this.height + ')')
        .call(bottomAxis);

      svg.append('g')
        .attr('class', styleClassAxisY)
        .call(leftAxis);

      resolve();
    })
  }

  private drawLine(svg: Selection<BaseType, any, HTMLElement, any>, x: any, y: any, data: DeckdeckgoLineChartData[], compare: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      let line: Area<DeckdeckgoLineChartData> = area<DeckdeckgoLineChartData>()
        .x((d: DeckdeckgoLineChartData) => x(d.when));

      if (this.area) {
        line.y0(this.height).y1((d: DeckdeckgoLineChartData) => compare ? y(d.compare) : y(d.value));
      } else {
        line.y((d: DeckdeckgoLineChartData) => compare ? y(d.compare) : y(d.value));
      }

      if (this.smooth) {
        line.curve(curveMonotoneX);
      }

      svg.append('path')
        .datum(data)
        .attr('class', compare ? 'area-compare' : 'area')
        .attr('d', line);

      resolve();
    })
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
