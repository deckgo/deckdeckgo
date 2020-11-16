import {Component, h, Host, State, Element, Prop, EventEmitter, Event, Watch, Method} from '@stencil/core';

import {parseMarkdown} from '../workers/markdown.worker';

@Component({
  tag: 'deckgo-markdown',
  styleUrl: 'deckdeckgo-markdown.scss',
  shadow: true,
})
export class DeckgoMdParser {
  @Element() el: HTMLElement;

  @Prop() editable: boolean = false;

  @State()
  private editing: boolean = false;

  @Event()
  private markdownDidChange: EventEmitter<HTMLElement>;

  private containerRef!: HTMLDivElement;

  private parseAfterUpdate: boolean = false;

  // deckgo-highlight styling options
  @Prop({reflect: true}) highlightLines: string;
  @Prop({reflect: true}) terminal: string = 'carbon';
  @Prop({reflect: true}) theme: string = 'dracula';

  async componentDidLoad() {
    await this.parseMarkdownInSlot();
  }

  async componentDidUpdate() {
    if (this.parseAfterUpdate) {
      await this.parseMarkdownInSlot();
      this.parseAfterUpdate = false;
    }
  }

  @Watch('highlightLines')
  @Watch('terminal')
  @Watch('theme')
  async onCodeOptionsChange() {
    await this.parseMarkdownInSlot();
  }

  @Method()
  async lazyLoadContent() {
    const imgs: NodeListOf<HTMLElement> = this.el.shadowRoot.querySelectorAll('deckgo-lazy-img');

    if (!imgs || imgs.length <= 0) {
      return;
    }

    // any because I rather only have the @deckgo/lazy-img component as peer dependency. Not against improvement though.
    const promises: Promise<void>[] = Array.from(imgs).map((element: HTMLElement) => (element as any).lazyLoad());

    if (!promises || promises.length <= 0) {
      return;
    }

    await Promise.all(promises);
  }

  @Method()
  async getContainer(): Promise<HTMLDivElement | undefined> {
    return this.containerRef;
  }

  private async parseMarkdownInSlot() {
    const mdContent: HTMLElement = this.el.querySelector("[slot='markdown']");

    if (mdContent) {
      const mdText = mdContent.innerText;

      const markdownHtmlContents: string = await parseMarkdown({
        mdText,
        code: {
          highlightLines: this.highlightLines,
          terminal: this.terminal,
          theme: this.theme,
        },
      });

      await this.parseMarkdown(markdownHtmlContents);
    }
  }

  private async parseMarkdown(mdContentHTML: string) {
    if (!this.containerRef) {
      return;
    }

    if (!mdContentHTML || mdContentHTML === undefined || mdContentHTML === '') {
      this.containerRef.children[0].innerHTML = '';
      return;
    }

    try {
      this.containerRef.children[0].innerHTML = '';

      const div: HTMLElement = document.createElement('div');

      if (div.childNodes) {
        this.containerRef.children[0].innerHTML = mdContentHTML;
      }
    } catch (err) {
      console.error(err);
    }
  }

  private applyMarkdown = async () => {
    await this.stopEditing();
    await this.parseMarkdownInSlot();
  };

  private startEditing(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.editable) {
        resolve();
        return;
      }

      this.editing = true;

      const markdownInSlot: HTMLElement = this.el.querySelector("[slot='markdown']");

      if (markdownInSlot) {
        setTimeout(() => {
          markdownInSlot.setAttribute('contentEditable', 'true');
          markdownInSlot.addEventListener('blur', this.applyMarkdown, {once: true});
          markdownInSlot.focus();
        }, 100);
      }

      resolve();
    });
  }

  private stopEditing(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.editing = false;

      const markdownInSlot: HTMLElement = this.el.querySelector("[slot='markdown']");

      if (markdownInSlot) {
        markdownInSlot.removeAttribute('contentEditable');
      }

      this.markdownDidChange.emit(this.el);

      resolve();
    });
  }

  render() {
    return (
      <Host class={{'deckgo-markdown-edit': this.editing}}>
        <div
          class="deckgo-markdown-container"
          ref={(el) => (this.containerRef = el as HTMLInputElement)}
          onMouseDown={() => this.startEditing()}
          onTouchStart={() => this.startEditing()}>
          <div class="markdown"></div>
          <slot name="markdown"></slot>
        </div>
      </Host>
    );
  }
}
