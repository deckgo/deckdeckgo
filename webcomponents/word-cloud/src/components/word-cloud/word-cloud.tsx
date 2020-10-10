import {Component, Prop, h, Host, Element, State, Event, EventEmitter} from '@stencil/core';

import {select} from 'd3-selection';
import cloud from 'd3-cloud';

import {draw} from '../utils/word-cloud-draw';

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

  private svgRef!: SVGGElement;

  private colors: string[] = Array.from({length: 5}, (_v, _i) => Math.floor(Math.random() * 16777215).toString(16));

  async componentDidLoad() {
    await this.updatePlaceholder();
    await this.wordCloud();
  }

  private async edit() {
    if (!this.editable) {
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
  }

  private async stopEditing() {
    this.editing = false;

    const wordsSlot: HTMLElement = this.el.querySelector(":scope > [slot='words']");

    if (wordsSlot) {
      wordsSlot.removeAttribute('contentEditable');

      this.wordCloudDidChange.emit(this.el);
    }
  }

  private async parseSlottedWords(): Promise<string[] | null> {
    const wordsSlot: HTMLElement = this.el.querySelector(":scope > [slot='words']");
    if (!wordsSlot || wordsSlot.innerText === '') {
      return null;
    }

    const words: RegExpMatchArray = wordsSlot.innerText.split(/[\s,]+/);

    if (!words || words.length <= 0) {
      return null;
    }

    return [...new Set(words)];
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
    if (!this.svgRef) {
      return;
    }

    const words: string[] | null = await this.parseSlottedWords();

    if (!words) {
      this.clearSVG();
      return;
    }

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
      .on('end', (words) => {
        this.clearSVG();
        draw(this.svgRef, words, this.width, this.height);
      });

    layout.start();
  }

  private getRandomColor(): string {
    const index: number = Math.floor(Math.random() * this.colors.length);
    return `#${this.colors[index]}`;
  }

  private async applyChanges() {
    await Promise.all([this.wordCloud(), this.stopEditing(), this.updatePlaceholder()]);
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
          <svg class="words" ref={(el) => (this.svgRef = el as SVGGElement)}></svg>
        </div>
      </Host>
    );
  }
}
