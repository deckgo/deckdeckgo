import {BaseType, select, Selection} from 'd3-selection';

export interface DeckdeckgoChart {
  draw(): Promise<void>;

  postCustomLoad(content: string | undefined);
}

export class DeckdeckgoChartUtils {
  static initSvg(el: HTMLElement, width: number, height: number): Selection<BaseType, any, HTMLElement, any> {
    let svg: Selection<BaseType, any, HTMLElement, any> = select(el.shadowRoot.querySelector('svg')).attr('width', width).attr('height', height);

    this.clearSvg(svg);

    return svg;
  }

  private static clearSvg(svg: Selection<BaseType, any, HTMLElement, any>) {
    if (svg) {
      svg.selectAll('*').remove();
    }
  }
}
