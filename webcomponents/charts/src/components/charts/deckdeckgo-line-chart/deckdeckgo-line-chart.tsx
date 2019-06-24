import {Component, Element, Method, Prop, Watch, h} from '@stencil/core';

import parse from 'date-fns/parse';
import isValid from 'date-fns/isValid';

import {BaseType, Selection} from 'd3-selection';
import {scaleLinear, scaleTime} from 'd3-scale';
import {extent} from 'd3-array';
import {Axis, axisBottom, axisLeft} from 'd3-axis';
import {Area, area, curveMonotoneX} from 'd3-shape';
import {transition} from 'd3-transition';
import {easeLinear} from 'd3-ease';

import {DeckdeckgoChart, DeckdeckgoChartUtils} from '../deckdeckgo-chart';

interface DeckdeckgoLineChartData {
  when: Date | number;
  value: number;
}

interface DeckdeckgoLineChartSerie {
  data: DeckdeckgoLineChartData[];
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

  @Prop({mutable: true}) width: number;
  @Prop({mutable: true}) height: number;

  @Prop() src: string;
  @Prop() separator: string = ';';

  // All supported date format: https://date-fns.org/v2.0.0-alpha.26/docs/parse
  @Prop() datePattern: string = 'yyyy-MM-dd';

  @Prop() marginTop: number = 32;
  @Prop() marginBottom: number = 64;
  @Prop() marginLeft: number = 32;
  @Prop() marginRight: number = 32;

  @Prop() yAxisDomain: string = DeckdeckgoLineChartAxisDomain.MAX;

  @Prop() smooth: boolean = true;
  @Prop() area: boolean = true;
  @Prop() ticks: number;
  @Prop() grid: boolean = false;

  @Prop() animation: boolean = false;
  @Prop() animationDuration: number = 1000;

  private svg: Selection<BaseType, any, HTMLElement, any>;
  private x: any;
  private y: any;

  private series: DeckdeckgoLineChartSerie[];
  private serieIndex: number = 0;

  async componentDidLoad() {
    await this.draw();
  }

  @Watch('src')
  async redraw() {
    await this.draw();
  }

  @Method()
  draw(width?: number, height?: number): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (width > 0) {
        this.width = width;
      }

      if (height > 0) {
        this.height = height;
      }

      if (!this.width || !this.height || !this.src) {
        resolve();
        return;
      }

      this.serieIndex = 0;

      this.svg = DeckdeckgoChartUtils.initSvg(this.el, (this.width + this.marginLeft + this.marginRight), (this.height + this.marginTop + this.marginBottom));
      this.svg = this.svg.append('g').attr('transform', 'translate(' + this.marginLeft + ',' + this.marginTop + ')');

      this.series = await this.fetchData();

      if (!this.series || this.series.length <= 0) {
        resolve();
        return;
      }

      await this.initAxis();

      await this.drawAxis();

      await this.drawLine(0);

      // All other series to compare if not animated
      if (this.series.length > 1 && !this.animation) {
        const promises = [];

        for (let i = 1; i < this.series.length; i++) {
          promises.push(this.drawLine(i));
        }

        await Promise.all(promises);
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

  private async prevNext(next: boolean) {
    if (!this.animation) {
      return;
    }

    if (!this.series || this.series.length <= 0) {
      return;
    }

    if (next && this.serieIndex + 1 < this.series.length) {
      this.serieIndex++;
      await this.drawLine(this.serieIndex);
    } else if (!next && this.serieIndex > 0) {
      this.serieIndex--;
      await this.drawLine(this.serieIndex);
    }
  }

  @Method()
  isBeginning(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!this.animation) {
        resolve(true);
        return;
      }

      resolve(this.serieIndex === 0);
    });
  }

  @Method()
  isEnd(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!this.animation) {
        resolve(true);
        return;
      }

      resolve(this.serieIndex === this.series.length - 1);
    });
  }

  private initAxis(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const firstSerieData: DeckdeckgoLineChartData[] = this.series[0].data;

      const isXAxisNumber: boolean = firstSerieData && firstSerieData.length > 0 && typeof firstSerieData[0].when === 'number';

      this.x = isXAxisNumber ? scaleLinear().range([0, this.width]) : scaleTime().range([0, this.width]);
      this.y = scaleLinear().range([this.height, 0]);

      this.x.domain(extent(firstSerieData, (d: DeckdeckgoLineChartData) => d.when));

      if (this.yAxisDomain === DeckdeckgoLineChartAxisDomain.EXTENT) {
        if (this.series.length > 1) {
          const minMaxValues: number[] = await this.findMinMaxValues();
          this.y.domain(minMaxValues);
        } else {
          this.y.domain(extent(firstSerieData, (d: DeckdeckgoLineChartData) => d.value));
        }
      } else {
        const minMaxValues: number[] = await this.findMinMaxValues();
        this.y.domain([0, minMaxValues[1]]);
      }

      resolve();
    });
  }

  private findMinMaxValues(): Promise<number[]> {
    return new Promise<number[]>(async (resolve) => {
      if (!this.series || this.series.length <= 0) {
        resolve([0, 0]);
        return;
      }

      const promises = [];
      for (let i = 0; i < this.series.length; i++) {
        promises.push(this.findMinMax(i));
      }

      const results: { min: number, max: number }[] = await Promise.all(promises);

      const minMax: { min: number, max: number } = await this.concatMinMax(results);

      resolve([minMax.min, minMax.max]);
    })
  }

  private concatMinMax(allMinMax: { min: number, max: number }[]): Promise<{ min: number, max: number }> {
    return new Promise<{ min: number, max: number }>((resolve) => {
      if (!allMinMax || allMinMax.length <= 0) {
        resolve({
          min: 0,
          max: 0
        });
        return;
      }

      // https://stackoverflow.com/a/30834687/5404186
      let min: number = allMinMax[0].min;
      let max: number = allMinMax[0].max;

      for (let i = 1; i < allMinMax.length; ++i) {
        if (allMinMax[i].max > max) {
          max = allMinMax[i].max;
        }

        if (allMinMax[i].min < min) {
          min = allMinMax[i].min;
        }
      }

      resolve({
        min: min,
        max: max
      });
    });
  }

  private findMinMax(index: number): Promise<{ min: number, max: number }> {
    return new Promise<{ min: number, max: number }>((resolve) => {
      // https://stackoverflow.com/a/30834687/5404186
      let min: number = this.series[index].data[0].value;
      let max: number = this.series[index].data[0].value;

      for (let i = 1; i < this.series[index].data.length; ++i) {
        if (this.series[index].data[i].value > max) {
          max = this.series[index].data[i].value;
        }

        if (this.series[index].data[i].value < min) {
          min = this.series[index].data[i].value;
        }
      }

      resolve({
        min: min,
        max: max
      });
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
        .call(bottomAxis)
        .selectAll('text')
        .attr('transform', 'translate(-10,0)rotate(-45)')
        .style('text-anchor', 'end');

      this.svg.append('g')
        .attr('class', styleClassAxisY)
        .call(leftAxis);

      resolve();
    })
  }

  private drawLine(index: number): Promise<void> {
    return new Promise<void>((resolve) => {
      let line: Area<DeckdeckgoLineChartData> = area<DeckdeckgoLineChartData>()
        .x((d: DeckdeckgoLineChartData) => this.x(d.when));

      if (this.area) {
        line.y0(this.height).y1((d: DeckdeckgoLineChartData) => this.y(d.value));
      } else {
        line.y((d: DeckdeckgoLineChartData) => this.y(d.value));
      }

      if (this.smooth) {
        line.curve(curveMonotoneX);
      }

      const data: DeckdeckgoLineChartData[] = this.series[index].data;

      if (this.animation) {
        this.drawAnimatedLine(data, index, line);
      } else {
        this.drawInstantLine(data, index, line);
      }

      resolve();
    })
  }

  private drawInstantLine(data: DeckdeckgoLineChartData[], index: number, line: Area<DeckdeckgoLineChartData>) {
    // Random hex color source: https://css-tricks.com/snippets/javascript/random-hex-color/

    this.svg.append('path')
      .datum(data)
      .attr('class', 'area')
      .style('fill', `var(--deckgo-chart-fill-color-${index}, #${Math.floor(Math.random()*16777215).toString(16)})`)
      .style('fill-opacity', `var(--deckgo-chart-fill-opacity-${index})`)
      .style('stroke', `var(--deckgo-chart-stroke-${index})`)
      .style('stroke-width', `var(--deckgo-chart-stroke-width-${index})`)
      .attr('d', line);
  }

  private drawAnimatedLine(data: DeckdeckgoLineChartData[], index: number, line: Area<DeckdeckgoLineChartData>) {
    const t = transition();

    const section: any = this.svg.selectAll('.area')
      .data([data], (d: DeckdeckgoLineChartData) => {
        return this.y(d.value)
      });

    section
      .enter()
      .append('path').merge(section)
      .style('fill', `var(--deckgo-chart-fill-color-${index}, #${Math.floor(Math.random()*16777215).toString(16)})`)
      .style('fill-opacity', `var(--deckgo-chart-fill-opacity-${index})`)
      .style('stroke', `var(--deckgo-chart-stroke-${index})`)
      .style('stroke-width', `var(--deckgo-chart-stroke-width-${index})`)
      .transition(t).duration(this.animationDuration).ease(easeLinear)
      .attr('class', 'area')
      .attr('d', line);
  }

  async fetchData(): Promise<DeckdeckgoLineChartSerie[]> {
    return new Promise<DeckdeckgoLineChartSerie[]>(async (resolve) => {
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

      const series: DeckdeckgoLineChartSerie[] = [];

      for (const line of lines) {
        const values: string[] = line.split(this.separator);

        if (values && values.length > 1) {

          const when: any = this.validDate(values[0]) ? parse(values[0], this.datePattern, new Date()) : parseInt(values[0]);

          if (when) {
            for (let i: number = 1; i < values.length; i++) {
              const value: number = parseInt(values[i]);

              if (!isNaN(value)) {
                let data: DeckdeckgoLineChartSerie = series && series.length >= i ? series[i - 1] : {data: []};

                if (!data.data || data.data.length <= 0) {
                  data.data = [];
                }

                data.data.push({
                  when: when,
                  value: parseInt(values[i])
                });

                if (series && series.length >= i) {
                  series[i - 1] = data;
                } else {
                  series.push(data);
                }
              }
            }
          }
        }
      }

      resolve(series);
    });
  }

  private validDate(value: string): boolean {
    return this.datePattern && isValid(parse(value, this.datePattern, new Date()));
  }

  render() {
    return <svg></svg>;
  }

}
