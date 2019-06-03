import {Component, Prop, Element, Method, Watch, h} from '@stencil/core';

import {BaseType, Selection} from 'd3-selection';
import {pie, arc, Pie, Arc, DefaultArcObject} from 'd3-shape';
import {schemePastel2} from 'd3-scale-chromatic';
import {ScaleOrdinal, scaleOrdinal} from 'd3-scale';

import {DeckdeckgoChart, DeckdeckgoChartUtils} from '../deckdeckgo-chart';

interface DeckdeckgoPieChartData {
  label?: string;
  value: number;
}

@Component({
  tag: 'deckgo-pie-chart',
  styleUrl: 'deckdeckgo-pie-chart.scss',
  shadow: true
})
export class DeckdeckgoPieChart implements DeckdeckgoChart {

  @Element() el: HTMLElement;

  @Prop() width: number;
  @Prop() height: number;

  // Specify a number for a donut chart
  @Prop() innerRadius: number = 0;

  // For example: ['#98abc5', '#8a89a6', '#7b6888', '#6b486b', '#a05d56', '#d0743c', '#ff8c00']
  @Prop() range: string[];

  @Prop() src: string;
  @Prop() separator: string = ';';

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

      let svg: Selection<BaseType, any, HTMLElement, any> = DeckdeckgoChartUtils.initSvg(this.el, this.width, this.height);

      const radius: number = Math.min(this.width, this.height) / 2;

      // Apply transformation to svg
      const g: Selection<any, any, any, any> = svg.append('g').attr('transform', 'translate(' + (this.width / 2) + ',' + (this.height / 2) + ')');

      // Define the arcs
      const myPath: Arc<any, DefaultArcObject> = arc().innerRadius(this.innerRadius).outerRadius(radius);

      const myPieData: any[] = await this.createPieData();

      await this.drawPie(g, myPieData, myPath);

      await this.appendLabel(g, myPieData, myPath);

      resolve();
    });
  }

  private drawPie(g: Selection<any, any, any, any>, myPieData: any[], myPath: Arc<any, DefaultArcObject>): Promise<void> {
    return new Promise<void>((resolve) => {
      // Load the data
      const myArcs: Selection<BaseType, any, HTMLElement, any> = g.selectAll('.arc')
        .data(myPieData)
        .enter()
        .append('g')
        .attr('class', 'arc');

      // Defined the colors
      const colors: ScaleOrdinal<string, string> = this.getColors();

      // Append to the path and apply colors
      myArcs.append('path').attr('d', myPath)
        .attr('fill', () => {
          return colors('' + Math.random())
        });

      resolve();
    });
  }

  private async createPieData(): Promise<any[]> {
    // Generate a pie chart
    const myPie: Pie<any, number | { valueOf(): number }> = pie().sort(null).value((d: any) => {
      return d.value
    });

    // Fetch data, csv to json
    const data: any = await this.fetchData();

    return myPie(data);
  }

  private appendLabel(g: Selection<any, any, any, any>, myPieData: any[], myPath: Arc<any, DefaultArcObject>): Promise<void> {
    return new Promise<void>((resolve) => {
      // Create a special arcs for the labels in order to always display them above the pie's slices
      const labelArcs: Selection<BaseType, any, HTMLElement, any> = g.selectAll('.text-arc')
        .data(myPieData)
        .enter()
        .append('g')
        .attr('class', 'text-arc');

      const text = labelArcs.append('text')
        .attr('transform', (d: DefaultArcObject) => {
          return 'translate(' + myPath.centroid(d) + ')';
        })
        .attr('dy', '.35em')
        .style('text-anchor', 'middle');

      text.append('tspan')
        .attr('x', 0)
        .attr('y', '-0.7em')
        .style('font-weight', 'bold')
        .text((d: any) => {
          return d.data.label ? d.data.label : '';
        });

      text.filter((d: any) => (d.endAngle - d.startAngle) > 0.25).append('tspan')
        .attr('x', 0)
        .attr('y', '0.7em')
        .text((d: any) => {
          return d.data.value + '%';
        });

      resolve();
    });
  }

  private getColors(): ScaleOrdinal<string, string> {
    const colors: ScaleOrdinal<string, string> = scaleOrdinal(schemePastel2);

    if (this.range && this.range.length > 0) {
      colors.range(this.range);
    }

    return colors;
  }

  async fetchData(): Promise<DeckdeckgoPieChartData[]> {
    return new Promise<DeckdeckgoPieChartData[]>(async (resolve) => {
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

      let results: DeckdeckgoPieChartData[] = [];
      lines.forEach((line: string) => {
        const values: string[] = line.split(this.separator);

        if (values && values.length >= 1) {

          // Source file could contains one or two columns
          let data: DeckdeckgoPieChartData = {
            value: parseInt(values.length > 1 ? values[1] : values[0])
          };

          // If two columns, we amend the fact that the first one is the label
          if (values.length > 1) {
            data.label = values[0];
          }

          if (!isNaN(data.value)) {
            results.push(data);
          }
        }
      });

      resolve(results);
    });
  }

  render() {
    return <svg></svg>;
  }
}
