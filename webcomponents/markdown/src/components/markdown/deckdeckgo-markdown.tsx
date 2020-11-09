import {Component, h, Host, State, Element, Prop} from '@stencil/core';

import {Remarkable} from 'remarkable';

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

  private containerRef!: HTMLDivElement;

  private parseAfterUpdate: boolean = false;

  private parser: Remarkable = new Remarkable();

  async componentDidLoad() {
    await this.parseMarkdownInSlot();
  }

  async componentDidUpdate() {
    if (this.parseAfterUpdate) {
      await this.parseMarkdownInSlot();
      this.parseAfterUpdate = false;
    }
  }

  private parseMarkdownInSlot(): Promise<void> {
    const mdContent: HTMLElement = this.el.querySelector("[slot='markdown']");

    if (mdContent) {
      const mdText = mdContent.innerText;
      const mdContentHTML = this.parser.render(mdText);
      this.parseMarkdown(mdContentHTML);
    } else {
      return Promise.resolve();
    }
  }

  private parseMarkdown(mdContentHTML: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      if (!this.containerRef) {
        resolve();
        return;
      }

      if (!mdContentHTML || mdContentHTML === undefined || mdContentHTML === '') {
        this.containerRef.children[0].innerHTML = '';
        resolve();
        return;
      }

      try {
        this.containerRef.children[0].innerHTML = '';

        const div: HTMLElement = document.createElement('div');

        try {
          if (div.childNodes) {
            this.containerRef.children[0].innerHTML = mdContentHTML;
          }
        } catch (err) {
          console.error(err);
        }

        resolve();
      } catch (err) {
        reject(err);
      }
    });
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
