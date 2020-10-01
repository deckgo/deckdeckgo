import { Component, Prop, h, Host, Element } from '@stencil/core';

import { select } from 'd3-selection';
import cloud from 'd3-cloud';

@Component({
  tag: 'deckgo-word-cloud',
  styleUrl: 'word-cloud.scss',
  shadow: true,
})
export class DeckdeckgoWordCloud {
  @Element() el: HTMLElement;

  @Prop() width: number = 500;
  @Prop() height: number = 500;
  @Prop() font: string = 'Impact';

  constructor() { }

  wordCloud() {
    const words = this.parseSlottedWords();

    const layout = cloud().size([this.width, this.height])
      .words(words.map(d => ({ text: d, size: 10 + Math.random() * 90, test: "haha" })))
      .padding(5)
      .font("Impact")
      .fontSize(d => d.size)
      .on("end", words => this.draw(words, this));

    layout.start();
  }

  draw(words, self: DeckdeckgoWordCloud) {

    select(self.el.shadowRoot.querySelector('svg'))
      .attr("width", self.width)
      .attr("height", self.height)
      .append("g")
      .attr("transform", "translate(" + self.width / 2 + "," + self.height / 2 + ")")
      .selectAll("text")
      .data(words)
      .enter().append("text")
      .style("font-size", d => d.size + "px")
      .style("font-family", self.font)
      .attr("text-anchor", "middle")
      .attr("transform", d => "translate(" + [d.x, d.y] + ")rotate(" + d.rotate + ")")
      .text(d => d.text);
  }

  componentDidLoad() {
    this.wordCloud();
  }

  componentDidUpdate() {
    this.wordCloud();
  }

  private parseSlottedWords(): string[] {
    const wordsContent: HTMLElement = this.el.querySelector("[slot='words']");

    console.log(wordsContent.innerText)
    return wordsContent.innerText.split(' ');
  }

  render() {
    return (
      <Host class={"word-cloud"}>
        <div style={{ 'display': 'none' }}>
          <slot name="words" />
        </div>
        <svg></svg>
      </Host>
    )
  }
}
