import {Component, Prop, Watch, Element, h, State, Host, Event, EventEmitter} from '@stencil/core';
import katex from 'katex';
import {extractMath, Segment} from 'extract-math';

@Component({
  tag: 'deckgo-math',
  styleUrl: 'deckdeckgo-math.scss',
  shadow: true
})
export class DeckdeckgoMath {
  @Element() el: HTMLElement;

  @Prop() editable: boolean = false;

  @State() editing: boolean = false;

  @Prop({reflectToAttr: true}) leqno: boolean = false;
  @Watch('leqno')
  async leqnoChanged() {
    await this.parseSlottedMath();
  }

  @Prop({reflectToAttr: true}) fleqn: boolean = false;
  @Watch('fleqn')
  async fleqnChanged() {
    await this.parseSlottedMath();
  }

  @Event() mathError!: EventEmitter<any>;
  @Event() private mathDidChange: EventEmitter<HTMLElement>;

  async componentDidLoad() {
    await this.parseSlottedMath();
  }
  async componentDidUpdate() {
    await this.parseSlottedMath();
  }
  private parseSlottedMath(): Promise<void> {
    const mathContent: HTMLElement = this.el.querySelector("[slot='math']");

    if (mathContent) {
      return this.parseMath(mathContent.innerHTML);
    } else {
      return new Promise<void>((resolve) => {
        resolve();
      });
    }
  }

  private parseMath(mathContentHTML: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-math-container');

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
            displayMode: segment.type === 'display',
            leqno: this.leqno,
            fleqn: this.fleqn
          });
        } catch (error) {
          if (error instanceof katex.ParseError) {
            // KaTeX can't parse the expression
            let message = ("Error in LaTeX '" + segment.raw + "': " + error.message)
              .replace(/&/g, '&amp;')
              .replace(/</g, '&lt;')
              .replace(/>/g, '&gt;');
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

  private applyMath = async () => {
    await this.stopEditing();

    await this.parseSlottedMath();
  };
  private edit(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.editable) {
        resolve();
        return;
      }

      this.editing = true;

      const slottedMath: HTMLElement = this.el.querySelector("[slot='math']");

      if (slottedMath) {
        setTimeout(() => {
          slottedMath.setAttribute('contentEditable', 'true');
          slottedMath.addEventListener('blur', this.applyMath, {once: true});

          slottedMath.focus();
        }, 100);
      }

      resolve();
    });
  }
  private stopEditing(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.editing = false;

      const slottedMath: HTMLElement = this.el.querySelector("[slot='math']");

      if (slottedMath) {
        slottedMath.removeAttribute('contentEditable');

        if (slottedMath.innerHTML) {
          slottedMath.innerHTML = slottedMath.innerHTML.trim();
        }

        this.mathDidChange.emit(this.el);
      }

      resolve();
    });
  }

  render() {
    return (
      <Host
        class={{
          'deckgo-math-edit': this.editing
        }}>
        <div class="deckgo-math-container" onMouseDown={() => this.edit()} onTouchStart={() => this.edit()}>
          <div class="math"></div>
          <slot name="math"></slot>
        </div>
      </Host>
    );
  }
}
