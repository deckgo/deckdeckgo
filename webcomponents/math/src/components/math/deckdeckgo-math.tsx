import {Component, Prop, Watch, Element, h, State, Host, Event, EventEmitter} from '@stencil/core';

import katex from 'katex';
import {extractMath, Segment} from 'extract-math';

/**
 * @slot math - The math expression to render
 */
@Component({
  tag: 'deckgo-math',
  styleUrl: 'deckdeckgo-math.scss',
  shadow: true
})
export class DeckdeckgoMath {
  @Element() el: HTMLElement;

  /**
   * To set the component has being editable (contenteditable will be applied on the slot on click)
   */
  @Prop() editable: boolean = false;

  /**
   * A collection of custom macros. Each macro is a property with a name like \name (written "\name" in JavaScript) which maps to a string that describes the expansion of the macro, or a function that accepts an instance of MacroExpander as first argument and returns the expansion as a string.
   */
  @Prop({reflect: true}) macros: string = `{"\\\\f":"f(#1)"}`;

  @State()
  private editing: boolean = false;

  /**
   * Emit the host element when modified
   */
  @Event()
  mathDidChange: EventEmitter<HTMLElement>;

  private parseAfterUpdate: boolean = false;

  private containerRef!: HTMLDivElement;

  async componentDidLoad() {
    await this.parseSlottedMath();
  }

  async componentDidUpdate() {
    if (this.parseAfterUpdate) {
      await this.parseSlottedMath();
      this.parseAfterUpdate = false;
    }
  }

  @Watch('macros')
  async macrosChanged() {
    this.parseAfterUpdate = true;

    await this.parseSlottedMath();
  }

  private parseSlottedMath(): Promise<void> {
    const mathContent: HTMLElement = this.el.querySelector("[slot='math']");

    if (mathContent) {
      return this.parseMath(mathContent.innerText);
    } else {
      return Promise.resolve();
    }
  }

  private parseMath(mathContentHTML: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      if (!this.containerRef) {
        resolve();
        return;
      }

      if (!mathContentHTML || mathContentHTML === undefined || mathContentHTML === '') {
        this.containerRef.children[0].innerHTML = '';

        resolve();
        return;
      }

      try {
        this.containerRef.children[0].innerHTML = '';

        const div: HTMLElement = document.createElement('div');

        try {
          div.innerHTML = await this.extractAndRenderMath(mathContentHTML);

          if (div.childNodes) {
            this.containerRef.children[0].append(...Array.from(div.childNodes));
          }
        } catch (err) {
          this.containerRef.children[0].innerHTML = mathContentHTML;
          console.error(err);
        }

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async extractAndRenderMath(mathContentHTML: string): Promise<string> {
    const segments: Segment[] = extractMath(mathContentHTML);

    if (!segments || (segments.length === 1 && !segments[0].math)) {
      return this.extract(segments[0].raw, segments[0].type);
    }

    let renderedHTML = '';

    segments.forEach((segment) => {
      if (segment.math) {
        renderedHTML += this.extract(segment.raw, segment.type);
      } else {
        renderedHTML += segment.value;
      }
    });

    return renderedHTML;
  }

  private extract(raw: string, type: 'text' | 'display' | 'inline') {
    return katex.renderToString(raw, {
      displayMode: type === 'display',
      leqno: false,
      fleqn: false,
      macros: this.macros ? JSON.parse(this.macros) : null,
      strict: 'warn',
      trust: false,
      throwOnError: true
    });
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
        <div
          class="deckgo-math-container"
          ref={(el) => (this.containerRef = el as HTMLInputElement)}
          onMouseDown={() => this.edit()}
          onTouchStart={() => this.edit()}>
          <div class="math"></div>
          <slot name="math"></slot>
        </div>
      </Host>
    );
  }
}
