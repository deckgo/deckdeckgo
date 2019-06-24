import {Component, Element, Method, Prop, Watch, h} from '@stencil/core';

import {DeckdeckgoChart, DeckdeckgoChartUtils} from '../deckdeckgo-chart';

import {BaseType, Selection} from 'd3-selection';
import {scaleBand, scaleLinear} from 'd3-scale';
import {max} from 'd3-array';
import {Axis, axisBottom, axisLeft} from 'd3-axis';
import {transition} from 'd3-transition';

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
  @Prop() marginBottom: number = 64;
  @Prop() marginLeft: number = 32;
  @Prop() marginRight: number = 32;

  @Prop() animation: boolean = false;
  @Prop() animationDuration: number = 1000;

  private svg: Selection<BaseType, any, HTMLElement, any>;
  private x0: any;
  private x1: any;
  private y: any;

  private data: DeckdeckgoBarChartData[];

  private barDataIndex: number = 0;

  async componentDidLoad() {
    await this.draw();
  }

  @Watch('width')
  @Watch('height')
  @Watch('src')
  async redraw() {
    this.barDataIndex = 0;

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

      if (!this.data || this.data.length <= 0) {
        resolve();
        return;
      }

      await this.initAxis();

      await this.drawAxis();

      await this.drawBars(0, 0);

      resolve();
    });
  }

  private async drawBars(index: number, animationDuration: number) {
    if (this.animation) {
      await this.drawAnimatedBars(index, animationDuration);
    } else {
      await this.drawInstantBars();
    }
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

    if (!this.data || this.data.length <= 0) {
      return;
    }

    if (next && this.barDataIndex + 1 < this.data.length) {
      this.barDataIndex++;
      await this.drawBars(this.barDataIndex, this.animationDuration);
    } else if (!next && this.barDataIndex > 0) {
      this.barDataIndex--;
      await this.drawBars(this.barDataIndex, this.animationDuration);
    }
  }

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

  @Method()
  isEnd(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!this.animation) {
        resolve(true);
        return;
      }

      resolve(this.barDataIndex === this.data.length - 1);
    });
  }

  private drawAxis(): Promise<void> {
    return new Promise<void>((resolve) => {
      const bottomAxis: Axis<any> = axisBottom(this.x0);
      const leftAxis: Axis<any> = axisLeft(this.y);

      this.svg.append('g')
        .attr('class', 'axis axis-x')
        .attr('transform', 'translate(0,' + this.height + ')')
        .call(bottomAxis)
        .selectAll('text')
        .attr('transform', 'translate(-10,0)rotate(-45)')
        .style('text-anchor', 'end');

      this.svg.append('g')
        .attr('class', 'axis axis-y')
        .call(leftAxis);

      resolve();
    })
  }

  private initAxis(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.x0 = scaleBand().rangeRound([0, this.width]);

      this.x1 = scaleBand().padding(0.05);
      this.y = scaleLinear().rangeRound([this.height, 0]);

      const xDomains = this.data[0].values.map((d) => {
        return d.key;
      });

      if (this.animation) {
        this.initAnimatedAxisX(xDomains);
      } else {
        this.initInstantAxisX(xDomains);
      }

      this.y.domain([0, max(this.data, (category) => {
        return max(category.values, (d) => {
          return d.value;
        });
      })]);

      resolve();
    });
  }

  private initAnimatedAxisX(xDomains: string[]) {
    this.x0.domain(xDomains).padding(0.05);
  }

  private initInstantAxisX(xDomains: string[]) {
    const categoriesNames = this.data.map((d) => {
      return d.label;
    });

    this.x0.paddingInner(0.1).domain(categoriesNames);
    this.x1.domain(xDomains).rangeRound([0, this.x0.bandwidth()]);
  }

  private drawAnimatedBars(index: number, animationDuration: number): Promise<void> {
    return new Promise<void>((resolve) => {
      const t = transition();

      const section: any = this.svg.selectAll('rect').data(this.data[index].values);

      section
        .enter()
        .append('rect')
        .merge(section)
        .attr('style', (d) => {
          return 'fill: var(--deckgo-chart-fill-color-' + d.key + '); fill-opacity: var(--deckgo-chart-fill-opacity-' + d.key + '); stroke: var(--deckgo-chart-stroke-' + d.key + '); stroke-width: var(--deckgo-chart-stroke-width-' + d.key + ')';
        })
        .transition(t).duration(animationDuration)
        .attr('x', (d) => {
          return this.x0(d.key);
        })
        .attr('y', (d) => {
          return this.y(d.value);
        })
        .attr('width', this.x0.bandwidth())
        .attr('height', (d) => {
          return this.height - this.y(d.value);
        });

      resolve();
    });
  }

  private drawInstantBars(): Promise<void> {
    return new Promise<void>((resolve) => {

      this.svg.append('g')
        .selectAll('g')
        .data(this.data)
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
    });
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
              });
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
