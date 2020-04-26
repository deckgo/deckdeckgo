import {Component, Prop, Watch, Element, h, Host, Event, EventEmitter} from '@stencil/core';
import katex from 'katex';
import {extractMath, Segment} from 'extract-math';
import {DeckDeckGoMathOptions} from '../declarations/deckdeckgo-math-options';

@Component({
  tag: 'deckgo-math-paragraph',
  styleUrl: 'deckdeckgo-math-paragraph.scss',
  shadow: true,
})
export class DeckdeckgoMathParagraph {
  @Element() el: HTMLElement;

  @Prop() options: DeckDeckGoMathOptions;

  @Watch('options')
  async optionsChanged() {
    await this.parseSlottedMathParagraph();
  }

  @Event() mathError!: EventEmitter<any>;

  async componentDidLoad() {
    await this.parseSlottedMathParagraph();
  }

  private parseSlottedMathParagraph(): Promise<void> {
    const mathContent: HTMLElement = this.el.querySelector("[slot='math']");

    if (mathContent) {
      return this.parseMathParagraph(mathContent.innerHTML);
    } else {
      return new Promise<void>((resolve) => {
        resolve();
      });
    }
  }

  private parseMathParagraph(mathContentHTML: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-math-paragraph-container');

      if (!mathContentHTML || mathContentHTML === undefined || mathContentHTML === '') {
        resolve();
        return;
      }
      if (container) {
        try {
          container.children[0].innerHTML = '';

          let div: HTMLElement = document.createElement('div');
          div.innerHTML = this.extractAndRenderMath(mathContentHTML);

          container.children[0].appendChild(div);
          resolve();
        } catch (err) {
          reject(err);
        }
      }
    });
  }

  private extractAndRenderMath(mathContentHTML: string) {
    var segments: Segment[] = extractMath(mathContentHTML);
    var renderedHTML = '';
    segments.map((segment) => {
      if (segment.math) {
        try {
          renderedHTML += katex.renderToString(segment.raw, {
            ...this.options,
            displayMode: segment.type === 'display',
          });
        } catch (error) {
          if (error instanceof katex.ParseError) {
            // KaTeX can't parse the expression
            let message = ("Error in LaTeX '" + segment.raw + "': " + error.message).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
            this.mathError.emit(message);
          } else {
            this.mathError.emit(error); //other error
          }
        }
      } else {
        renderedHTML += segment.value;
      }
    });
    return renderedHTML;
  }

  render() {
    return (
      <Host>
        <div class="deckgo-math-paragraph-container">
          <div></div>
          <slot name="math"></slot>
        </div>
      </Host>
    );
  }
}
