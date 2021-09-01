import {Component, Prop, Element, Method, Watch, h, Host, Event, EventEmitter} from '@stencil/core';

import {BaseType, Selection} from 'd3-selection';
import {pie, arc, Pie, Arc, DefaultArcObject} from 'd3-shape';

import {DeckdeckgoChart, DeckdeckgoChartUtils} from '../deckdeckgo-chart';
import 'd3-transition';
import {ascending} from 'd3-array';

interface DeckdeckgoPieChartDataValue {
  label: string;
  value: number;
  randomFillColor: string;
  key: number;
}

interface DeckdeckgoPieChartData {
  values: DeckdeckgoPieChartDataValue[];
}

@Component({
  tag: 'deckgo-pie-chart',
  styleUrl: 'deckdeckgo-pie-chart.scss',
  shadow: true,
})
export class DeckdeckgoPieChart implements DeckdeckgoChart {
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
   * The margin top of the chart in pixel
   */
  @Prop() marginTop: number = 8;
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
   * The inner radius of the pie
   */
  @Prop() innerRadius: number = 0;

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
   * Display multiple graphs and animate the transition between these
   */
  @Prop() animation: boolean = false;
  /**
   * Duration of the transition between graphs
   */
  @Prop() animationDuration: number = 1000;

  /**
   * The event to be processed to load the data if you are using a custom loader.
   * @private
   */
  @Event()
  chartCustomLoad: EventEmitter<string>;

  private svg: Selection<BaseType, any, HTMLElement, any>;
  private myPath: Arc<any, DefaultArcObject>;

  private data: DeckdeckgoPieChartData[];

  private pieDataIndex: number = 0;

  private randomColors: string[] | undefined;

  async componentDidLoad() {
    await this.draw();
  }

  @Watch('src')
  async redraw() {
    await this.draw();
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

      if (!this.width || !this.height || !this.src) {
        resolve();
        return;
      }

      this.pieDataIndex = 0;

      const maxWidth: number = this.width > this.marginLeft + this.marginRight ? this.width - this.marginLeft - this.marginRight : this.width;
      const maxHeight: number = this.height > this.marginTop + this.marginBottom ? this.height - this.marginTop - this.marginBottom : this.height;

      this.svg = DeckdeckgoChartUtils.initSvg(this.el, maxWidth, maxHeight);
      this.svg = this.svg.append('g').attr('transform', 'translate(' + maxWidth / 2 + ',' + maxHeight / 2 + ')');

      const radius: number = Math.min(maxWidth, maxHeight) / 2;

      this.myPath = arc().innerRadius(this.innerRadius).outerRadius(radius);

      this.data = await this.fetchData();

      if (!this.data || this.data.length <= 0) {
        resolve();
        return;
      }

      await this.drawPie(0, 0);

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

    if (!this.data || this.data.length <= 0) {
      return;
    }

    if (next && this.pieDataIndex + 1 < this.data.length) {
      this.pieDataIndex++;
      await this.drawPie(this.pieDataIndex, this.animationDuration);
    } else if (!next && this.pieDataIndex > 0) {
      this.pieDataIndex--;
      await this.drawPie(this.pieDataIndex, this.animationDuration);
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

      resolve(this.pieDataIndex === 0);
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

      resolve(this.pieDataIndex === this.data.length - 1);
    });
  }

  private async drawPie(index: number, animationDuration: number) {
    const pie: any[] = await this.createPieData(this.data[index]);

    await this.drawPieForData(pie, animationDuration);

    setTimeout(async () => {
      await this.appendLabel(pie);
    }, animationDuration);
  }

  private drawPieForData(myPieData: any[], animationDuration: number): Promise<void> {
    return new Promise<void>((resolve) => {
      const section: any = this.svg.selectAll('path').data(myPieData);

      section
        .enter()
        .append('path')
        .merge(section)
        .attr('style', (d) => {
          return (
            'fill: var(--deckgo-chart-fill-color-' +
            d.data.key +
            ', ' +
            (d.data.randomFillColor ? `#${d.data.randomFillColor}` : '') +
            '); fill-opacity: var(--deckgo-chart-fill-opacity-' +
            d.data.key +
            '); stroke: var(--deckgo-chart-stroke-' +
            d.data.key +
            '); stroke-width: var(--deckgo-chart-stroke-width-' +
            d.data.key +
            ')'
          );
        })
        .transition()
        .duration(animationDuration)
        .attr('d', this.myPath);

      section.exit().remove();

      resolve();
    });
  }

  private async createPieData(data: any): Promise<any[]> {
    return new Promise<any[]>(async (resolve) => {
      // Generate a pie chart
      const myPie: Pie<any, number | {valueOf(): number}> = pie()
        .value((d: any) => {
          return d.value;
        })
        .sort((a: any, b: any) => {
          return ascending(a.label, b.label);
        });

      const result: any[] = myPie(data.values);

      resolve(result);
    });
  }

  private appendLabel(myPieData: any[]): Promise<void> {
    return new Promise<void>((resolve) => {
      this.svg.selectAll('.text-arc').remove();

      // Create a special arcs for the labels in order to always display them above the pie's slices
      const labelArcs: Selection<SVGElement, any, BaseType, any> = this.svg
        .selectAll('.text-arc')
        .data(myPieData)
        .enter()
        .append('g')
        .attr('class', 'text-arc');

      const text = labelArcs
        .append('text')
        .attr('transform', (d: DefaultArcObject) => {
          return 'translate(' + this.myPath.centroid(d) + ')';
        })
        .attr('dy', '.35em')
        .style('text-anchor', 'middle');

      text
        .append('tspan')
        .attr('x', 0)
        .attr('y', '-0.7em')
        .style('font-weight', 'bold')
        .text((d: any) => {
          return d.data.label ? d.data.label : '';
        });

      text
        .filter((d: any) => d.endAngle - d.startAngle > 0.25)
        .append('tspan')
        .attr('x', 0)
        .attr('y', '0.7em')
        .text((d: any) => {
          return d.data.value + '%';
        });

      resolve();
    });
  }

  fetchData(): Promise<DeckdeckgoPieChartData[]> {
    return new Promise<DeckdeckgoPieChartData[]>(async (resolve) => {
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

      let results: DeckdeckgoPieChartData[] = await this.loadContent(content);

      resolve(results);
    });
  }

  @Method()
  async postCustomLoad(content: string | undefined) {
    this.data = await this.loadContent(content);

    if (!this.data || this.data.length <= 0) {
      return;
    }

    await this.drawPie(0, 0);
  }

  private loadContent(content: string | undefined): Promise<DeckdeckgoPieChartData[]> {
    return new Promise<DeckdeckgoPieChartData[]>(async (resolve) => {
      if (!content || content === undefined) {
        resolve([]);
        return;
      }

      const lines: string[] = content.split('\n');

      if (!lines || lines.length <= 0) {
        resolve([]);
        return;
      }

      let results: DeckdeckgoPieChartData[] = [];

      lines.forEach((line: string, lineIndex: number) => {
        const values: string[] = line.split(this.separator);

        if (values && values.length >= 2) {
          if (!this.randomColors || this.randomColors.length !== lines.length) {
            console.log('new');
            this.randomColors = Array.from({length: lines.length}, (_v, _i) => Math.floor(Math.random() * 16777215).toString(16));
          }

          const label: string = values[0];

          const pieData: DeckdeckgoPieChartDataValue = {
            label: label,
            value: parseInt(values[1]),
            randomFillColor: this.randomColors?.[lineIndex] || undefined,
            key: lineIndex + 1,
          };

          if (!isNaN(pieData.value)) {
            if (results.length <= 0) {
              results.push({
                values: [],
              });
            }

            results[0].values.push(pieData);
          }

          if (values.length > 2) {
            for (let i = 2; i < values.length; i++) {
              const tmp: number = parseInt(values[i]);
              if (!isNaN(tmp)) {
                if (results.length < i) {
                  results.push({
                    values: [],
                  });
                }

                const pieData: DeckdeckgoPieChartDataValue = {
                  label: label,
                  value: parseInt(values[i]),
                  randomFillColor: this.randomColors.length >= i ? this.randomColors[lineIndex] : undefined,
                  key: lineIndex + 1,
                };

                results[i - 1].values.push(pieData);
              }
            }
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
