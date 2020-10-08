import {Component, Prop, h, Host, Element, State, Event, EventEmitter} from '@stencil/core';

import {EnterElement, select, Selection} from 'd3-selection';
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

  @State()
  private editing: boolean = false;

  @Event()
  private wordCloudDidChange: EventEmitter<HTMLElement>;

  private containerRef!: HTMLDivElement;

  private colors: string[] = Array.from({length: 5}, (_v, _i) => Math.floor(Math.random() * 16777215).toString(16));

  async componentDidLoad() {
    await this.updatePlaceholder();
    await this.wordCloud();
  }

  private edit(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.editable) {
        resolve();
        return;
      }

      this.editing = true;

      const wordsSlot: HTMLElement = this.el.querySelector(":scope > [slot='words']");

      if (wordsSlot) {
        setTimeout(() => {
          wordsSlot.setAttribute('contentEditable', 'true');
          wordsSlot.addEventListener('blur', () => this.applyChanges(), {once: true});

          wordsSlot.focus();
        }, 100);
      }

      resolve();
    });
  }

  private stopEditing(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.editing = false;

      const wordsSlot: HTMLElement = this.el.querySelector(":scope > [slot='words']");

      if (wordsSlot) {
        wordsSlot.removeAttribute('contentEditable');

        this.wordCloudDidChange.emit(this.el);
      }

      resolve();
    });
  }

  private async parseSlottedWords(): Promise<string[]> {
    const wordsSlot: HTMLElement = this.el.querySelector(":scope > [slot='words']");
    if (!wordsSlot) {
      return [];
    }

    return wordsSlot.innerText.split(' ');
  }

  private updatePlaceholder() {
    return new Promise<void>((resolve) => {
      const wordsSlot: HTMLElement = this.el.querySelector(":scope > [slot='words']");
      this.containerRef.children[0].innerHTML = '';

      const div: HTMLElement = document.createElement('div');

      div.innerHTML = wordsSlot.innerHTML;

      if (div.childNodes) {
        this.containerRef.children[0].append(...Array.from(div.childNodes));
      }

      resolve();
    });
  }

  private clearSVG() {
    select(this.el.shadowRoot.querySelector('svg')).selectAll('*').remove();
  }

  private async wordCloud() {
    const words: string[] = await this.parseSlottedWords();

    const layout = cloud()
      .size([this.width, this.height])
      .words(
        words.map((d: string, index: number) => ({
          text: d,
          size: 10 + Math.random() * 110,
          color: `var(--deckgo-word-count-fill-color-${index}, ${this.getRandomColor()})`,
        }))
      )
      .rotate(() => ~~(Math.random() * 2) * 90)
      .padding(5)
      .fontSize((d) => d.size)
      .on('end', (words) => this.draw(words, this));

    layout.start();
  }

  private draw(words, self: DeckdeckgoWordCloud) {
    self.clearSVG();

    const selection: Selection<EnterElement, any, SVGGElement, any> = select(self.el.shadowRoot.querySelector('svg'))
      .attr('width', self.width)
      .attr('height', self.height)
      .append('g')
      .attr('transform', 'translate(' + self.width / 2 + ',' + self.height / 2 + ')')
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

  private getRandomColor(): string {
    const index: number = Math.floor(Math.random() * this.colors.length);
    return `#${this.colors[index]}`;
  }

  private async applyChanges() {
    this.wordCloud();
    await this.stopEditing();
    await this.updatePlaceholder();
  }

  render() {
    return (
      <Host
        class={{
          'deckgo-word-cloud-edit': this.editing,
        }}>
        <div
          class="deckgo-word-cloud-container"
          ref={(el) => (this.containerRef = el as HTMLInputElement)}
          onMouseDown={() => this.edit()}
          onTouchStart={() => this.edit()}>
          <div class="placeholder"></div>
          <slot name="words" />
          <svg class="words"></svg>
        </div>
      </Host>
    );
  }
}
