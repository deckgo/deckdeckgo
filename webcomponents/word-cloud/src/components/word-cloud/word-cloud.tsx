import { Component, Prop, h, Host, Element, State } from '@stencil/core';

import { select } from 'd3-selection';
import cloud from 'd3-cloud';

@Component({
  tag: 'deckgo-word-cloud',
  styleUrl: 'word-cloud.scss',
  shadow: true,
})
export class DeckdeckgoWordCloud {
  @Element() el: HTMLElement;

  @Prop() editable: boolean = false;
  @Prop() width: number = 500;
  @Prop() height: number = 500;
  @Prop() font: string = 'Impact';

  @State()
  private editing: boolean = false;

  constructor() {}

  componentDidLoad() {
    this.wordCloud();
  }

  componentDidUpdate() {
    this.clearSVG();
    this.wordCloud();
  }

  private edit(): Promise<void> {
    return new Promise<void>(resolve => {
      if (!this.editable) {
        resolve();
        return;
      }

      this.editing = true;

      const wordsSlot: HTMLElement = this.el.querySelector("[slot='words']");

      if (wordsSlot) {
        setTimeout(() => {
          wordsSlot.setAttribute('contentEditable', 'true');
          wordsSlot.addEventListener('blur', () => this.applyChanges(), { once: true });

          wordsSlot.focus();
        }, 100);
      }

      resolve();
    });
  }

  private stopEditing(): Promise<void> {
    return new Promise<void>(resolve => {
      this.editing = false;

      const wordsSlot: HTMLElement = this.el.querySelector("[slot='words']");

      if (wordsSlot) {
        wordsSlot.removeAttribute('contentEditable');
      }

      resolve();
    });
  }

  wordCloud() {
    const words = this.parseSlottedWords();

    const layout = cloud()
      .size([this.width, this.height])
      .words(words.map(d => ({ text: d, size: 10 + Math.random() * 100, color: this.getRandomColor() })))
      .rotate(() => ~~(Math.random() * 2) * 90)
      .padding(5)
      .font('Impact')
      .fontSize(d => d.size)
      .on('end', words => this.draw(words, this));

    layout.start();
  }

  private clearSVG() {
    select(this.el.shadowRoot.querySelector('svg')).select('g').remove('text');
  }

  private draw(words, self: DeckdeckgoWordCloud) {
    select(self.el.shadowRoot.querySelector('svg'))
      .attr('width', self.width)
      .attr('height', self.height)
      .append('g')
      .attr('transform', 'translate(' + self.width / 2 + ',' + self.height / 2 + ')')
      .selectAll('text')
      .data(words)
      .enter()
      .append('text')
      .style('font-size', d => d.size + 'px')
      .style('fill', d => d.color)
      .style('font-family', self.font)
      .attr('text-anchor', 'middle')
      .attr('transform', d => 'translate(' + [d.x, d.y] + ')rotate(' + d.rotate + ')')
      .text(d => d.text);
  }

  private getRandomColor() {
    const colors = ['#6114E5', '#000000', '#4E7224', '#C43636', '#7136C4', '#76E514'];
    const index = Math.floor(Math.random() * colors.length) + 1;
    return colors[index];
  }

  private parseSlottedWords(): string[] {
    const wordsSlot: HTMLElement = this.el.querySelector("[slot='words']");
    if (!wordsSlot) {
      return [];
    }
    return wordsSlot.innerText.split(' ');
  }

  private async applyChanges() {
    this.clearSVG();
    this.wordCloud();
    await this.stopEditing();
  }

  render() {
    return (
      <Host
        class={{
          'deckgo-word-cloud-edit': this.editing,
          '"word-cloud': true,
        }}
      >
        <div class="deckgo-word-cloud-container" onMouseDown={() => this.edit()} onTouchStart={() => this.edit()}>
          <slot name="words" />
          <svg class="words"></svg>
        </div>
      </Host>
    );
  }
}
