import {Component, Element, Method, Prop, Watch, h} from '@stencil/core';

import {DeckdeckgoChart, DeckdeckgoChartUtils} from '../deckdeckgo-chart';

import {BaseType, Selection} from 'd3-selection';
import {scaleBand, scaleLinear} from 'd3-scale';
import {max} from 'd3-array';
import {Axis, axisBottom, axisLeft} from 'd3-axis';

interface DeckdeckgoBarChartDataValue {
  key: string;
  value: number;
}

interface DeckdeckgoBarChartData {
  label: any;
  values: DeckdeckgoBarChartDataValue[];
}

@Component({
  tag: 'deckgo-bar-chart',
  styleUrl: 'deckdeckgo-bar-chart.scss',
  shadow: true
})
export class DeckdeckgoBarChart implements DeckdeckgoChart {

  @Element() el: HTMLElement;

  @Prop() width: number;
  @Prop() height: number;

  @Prop() src: string;
  @Prop() separator: string = ';';

  @Prop() marginTop: number = 32;
  @Prop() marginBottom: number = 32;
  @Prop() marginLeft: number = 32;
  @Prop() marginRight: number = 32;

  private svg: Selection<BaseType, any, HTMLElement, any>;
  private x0: any;
  private x1: any;
  private y: any;

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

      const data: DeckdeckgoBarChartData[] = await this.fetchData();

      await this.initAxis(data);

      await this.drawAxis();

      await this.drawBars(data);

      resolve();
    });
  }

  private drawAxis(): Promise<void> {
    return new Promise<void>((resolve) => {
      const bottomAxis: Axis<any> = axisBottom(this.x0);
      const leftAxis: Axis<any> = axisLeft(this.y);

      this.svg.append('g')
        .attr('class', 'axis axis-x')
        .attr('transform', 'translate(0,' + this.height + ')')
        .call(bottomAxis);

      this.svg.append('g')
        .attr('class', 'axis axis-y')
        .call(leftAxis);

      resolve();
    })
  }

  private initAxis(data: DeckdeckgoBarChartData[]): Promise<void> {
    return new Promise<void>((resolve) => {
      this.x0 = scaleBand().rangeRound([0, this.width]).paddingInner(0.1);

      this.x1 = scaleBand().padding(0.05);
      this.y = scaleLinear().rangeRound([this.height, 0]);

      const categoriesNames = data.map((d) => {
        return d.label;
      });
      const rateNames = data[0].values.map((d) => {
        return d.key;
      });

      this.x0.domain(categoriesNames);
      this.x1.domain(rateNames).rangeRound([0, this.x0.bandwidth()]);
      this.y.domain([0, max(data, (categorie) => {
        return max(categorie.values, (d) => {
          return d.value;
        });
      })]);

      resolve();
    });
  }

  private drawBars(data: DeckdeckgoBarChartData[]): Promise<void> {
    return new Promise<void>((resolve) => {

      this.svg.append('g')
        .selectAll('g')
        .data(data)
        .enter().append('g')
        .attr('transform', (d) => {
          return 'translate(' + this.x0(d.label) + ',0)';
        })
        .selectAll('rect')
        .data((d) => {
          return d.values;
        })
        .enter().append('rect')
        .attr('x', (d) => {
          return this.x1(d.key);
        })
        .attr('y', (d) => {
          return this.y(d.value);
        })
        .attr('width', this.x1.bandwidth())
        .attr('height', (d) => {
          return this.height - this.y(d.value);
        })
        .attr('style', (d) => {
          return 'fill: var(--deckgo-chart-fill-color-' + d.key + '); fill-opacity: var(--deckgo-chart-fill-opacity-' + d.key + '); stroke: var(--deckgo-chart-stroke-' + d.key + '); stroke-width: var(--deckgo-chart-stroke-width-' + d.key + ')';
        });

      resolve();
    })
  }

  async fetchData(): Promise<DeckdeckgoBarChartData[]> {
    return new Promise<DeckdeckgoBarChartData[]>(async (resolve) => {
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

      let results: DeckdeckgoBarChartData[] = [];
      lines.forEach((line: string) => {
        const values: string[] = line.split(this.separator);

        if (values && values.length >= 2) {

          let dataValues: DeckdeckgoBarChartDataValue[] = [];
          for (let i = 1; i < values.length; i++) {
            const tmp: number = parseInt(values[i]);
            if (!isNaN(tmp)) {
              dataValues.push({
                key: '' + i,
                value: tmp
              })
            }
          }

          results.push({
            label: values[0],
            values: dataValues
          });
        }
      });

      resolve(results);
    });
  }

  render() {
    return <svg></svg>;
  }

}
