import {EnterElement, select, Selection} from 'd3-selection';

export function draw(svg: SVGElement, words, width: number, height: number) {
  const selection: Selection<EnterElement, any, SVGGElement, any> = select(svg)
    .attr('width', width)
    .attr('height', height)
    .append('g')
    .attr('transform', 'translate(' + width / 2 + ',' + height / 2 + ')')
    .selectAll('text')
    .data(words)
    .enter();

  selection
    .append('text')
    .style('font-size', (d) => d.size + 'px')
    .style('fill', (d) => d.color)
    .attr('text-anchor', 'middle')
    .attr('transform', (d) => 'translate(' + [d.x, d.y] + ')rotate(' + d.rotate + ')')
    .text((d) => d.text);
}
