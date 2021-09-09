import {Component, Prop, h, Host, Element, State, Event, EventEmitter, Method} from '@stencil/core';

import type {DeckdeckgoComponent} from '@deckdeckgo/slide-utils';

import {debounce} from '@deckdeckgo/utils';

import {select} from 'd3-selection';
import cloud from 'd3-cloud';

import {draw} from '../utils/word-cloud-draw';

/**
 * @slot words - The list of words to render in a cloud
 */
@Component({
  tag: 'deckgo-word-cloud',
  styleUrl: 'word-cloud.scss',
  shadow: true
})
export class DeckdeckgoWordCloud implements DeckdeckgoComponent {
  @Element() el: HTMLElement;

  /**
   * To set the component has being editable (contenteditable will be applied on the slot on click)
   */
  @Prop()
  editable: boolean = false;

  /**
   * Margin top in pixels
   */
  @Prop() marginTop: number = 32;
  /**
   * Margin bottom in pixels
   */
  @Prop() marginBottom: number = 32;
  /**
   * Margin left in pixels
   */
  @Prop() marginLeft: number = 32;
  /**
   * Margin right in pixels
   */
  @Prop() marginRight: number = 32;

  @State()
  private editing: boolean = false;

  /**
   * Emit the host element when modified
   * @private
   */
  @Event()
  private wordCloudDidChange: EventEmitter<HTMLElement>;

  private containerRef!: HTMLDivElement;

  private svgRef!: SVGGElement;

  private colors: string[] | undefined;

  private width: number;
  private height: number;

  private editWords: string;

  async componentDidLoad() {
    this.initWindowResize();

    await this.initSize();

    await this.updatePlaceholder();
    await this.wordCloud();
  }

  disconnectedCallback() {
    if (window) {
      window.removeEventListener('resize', debounce(this.onResizeContent, 500));
    }
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onResizeContent, 500));
    }
  }

  private onResizeContent = async () => {
    await this.resizeReload();
  };

  /**
   * Call the load and resize of the word cloud
   */
  @Method()
  lazyLoadContent(): Promise<void> {
    return this.resizeReload();
  }

  private async resizeReload() {
    await this.initSize();
    await this.wordCloud();
  }

  private async initSize() {
    const style: CSSStyleDeclaration | undefined = window ? window.getComputedStyle(this.el) : undefined;

    const width: number = style && parseInt(style.width) > 0 ? parseInt(style.width) : this.el.offsetWidth;
    const height: number = style && parseInt(style.height) > 0 ? parseInt(style.height) : this.el.offsetHeight;

    this.width = width - this.marginLeft - this.marginRight;
    this.height = height - this.marginTop - this.marginBottom;
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

    const sizeMatch: boolean =
      this.svgRef.getAttribute('width') === `${this.width}` && this.svgRef.getAttribute('height') === `${this.height}`;

    if (words.join('') === this.editWords && sizeMatch) {
      // We do not repaint if the list of words did not change
      return;
    }

    if (!this.colors || this.colors.length !== words.length) {
      this.colors = Array.from({length: words.length}, (_v, _i) => Math.floor(Math.random() * 16777215).toString(16));
    }

    const layout = cloud()
      .size([this.width, this.height])
      .words(
        words.map((d: string, index: number) => ({
          text: d,
          size: 10 + Math.random() * 110,
          color: `var(--deckgo-word-count-fill-color-${index}, #${this.colors?.[index]})`
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

    this.editWords = words?.join('');
  }

  private async applyChanges() {
    await Promise.all([this.wordCloud(), this.stopEditing(), this.updatePlaceholder()]);
  }

  render() {
    return (
      <Host
        class={{
          'deckgo-word-cloud-edit': this.editing
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
