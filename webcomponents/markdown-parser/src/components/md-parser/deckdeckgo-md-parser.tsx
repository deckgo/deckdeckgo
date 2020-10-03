// import { Component, Prop, h } from '@stencil/core';
import { Component, h, Host, State, Element, Prop, Event, EventEmitter } from '@stencil/core';

import { Remarkable } from 'remarkable';

@Component({
  tag: 'deckgo-md-parser',
  styleUrl: 'deckdeckgo-md-parser.scss',
  shadow: true,
})
export class DeckgoMdParser {
  @Element() el: HTMLElement;

  @Prop() editable: boolean = false;

  @State()
  private editing: boolean = false;

  @Event()
  private mathDidChange: EventEmitter<HTMLElement>;

  private containerRef!: HTMLDivElement;

  private parser = new Remarkable();

  private parseMarkdownInSlot(): Promise<void> {
    const mdContent: HTMLElement = this.el.querySelector("[slot='markdown']");

    if (mdContent) {
      const mdText = mdContent.innerText;

      console.log(this.parser.render(mdText));

      return new Promise<void>(async resolve => {
        // if (!this.containerRef) {
        //   resolve();
        //   return;
        // }

        if (!mdText || mdText === undefined || mdText === '') {
          console.log('som tu hehe');
          this.containerRef.children[0].innerHTML = mdText;

          resolve();
          return;
        }
        resolve();

        // try {
        //   this.containerRef.children[0].innerHTML = '';

        //   const div: HTMLElement = document.createElement('div');

        //   try {
        //     if (div.childNodes) {
        //       this.containerRef.children[0].append(...Array.from(div.childNodes));
        //     }
        //   } catch (err) {
        //     console.error(err);
        //   }

        //   resolve();
        // } catch (err) {
        //   reject(err);
        // }
      });
    } else {
      return Promise.resolve();
    }
  }

  private applyMarkdown = async () => {
    await this.stopEditing();

    await this.parseMarkdownInSlot();
  };

  private edit(): Promise<void> {
    return new Promise<void>(resolve => {
      if (!this.editable) {
        resolve();
        return;
      }

      this.editing = true;

      const markdownInSlot: HTMLElement = this.el.querySelector("[slot='markdown']");

      if (markdownInSlot) {
        setTimeout(() => {
          markdownInSlot.setAttribute('contentEditable', 'true');
          markdownInSlot.addEventListener('blur', this.applyMarkdown, { once: true });

          markdownInSlot.focus();
        }, 100);
      }

      resolve();
    });
  }

  private stopEditing(): Promise<void> {
    return new Promise<void>(resolve => {
      this.editing = false;

      const markdownInSlot: HTMLElement = this.el.querySelector("[slot='markdown']");

      if (markdownInSlot) {
        markdownInSlot.removeAttribute('contentEditable');

        if (markdownInSlot.innerHTML) {
          markdownInSlot.innerHTML = markdownInSlot.innerHTML.trim();
        }

        this.mathDidChange.emit(this.el);
      }

      resolve();
    });
  }

  render() {
    return (
      <Host class={{ 'deckgo-math-edit': this.editing }}>
        <div class="deckgo-markdown-container" ref={el => (this.containerRef = el as HTMLInputElement)} onMouseDown={() => this.edit()} onTouchStart={() => this.edit()}>
          <div class="markdown"></div>
          <slot name="markdown"></slot>
        </div>
      </Host>
    );
  }
}
