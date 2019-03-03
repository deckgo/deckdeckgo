import {Component, Element, Prop, State, Listen} from '@stencil/core';

import {ContentType} from '../../types/inline-editor/deckdeckgo-inline-editor-types';

@Component({
  tag: 'deckgo-inline-editor',
  styleUrl: 'deckdeckgo-inline-editor.scss',
  shadow: true
})
export class DeckdeckgoInlineEditor {

  @Element() el: HTMLElement;

  @Prop()
  initType: string;

  @State()
  private type: ContentType = ContentType.P;

  @Prop()
  toolbarOffsetHeight: number;

  @State()
  private toolsActivated: boolean = false;

  private selection: Selection = null;

  componentWillLoad() {
    if (this.initType) {
      this.type = ContentType[this.initType.toUpperCase()];
    }
  }

  @Listen('document:mouseup', {passive: true})
  async mouseup($event: MouseEvent) {
    await this.displayTools($event);
  }

  @Listen('document:touchend', {passive: true})
  async touchend($event: TouchEvent) {
    await this.displayTools($event);
  }

  @Listen('document:keydown', {passive: true})
  async keydown($event: KeyboardEvent) {
    if ($event && ($event.key.toLowerCase() === 'backspace' || $event.key.toLowerCase() === 'delete')) {
      await this.reset(false);
    }
  }

  private displayTools($event: MouseEvent | TouchEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const selection: Selection = await this.getSelection();

      if (!selection || selection.toString().length <= 0) {
        await this.reset(false);
        resolve();
        return;
      }

      // Quirk when use click at the begin of the selection after having already selected something
      if (this.selection && selection.toString() === this.selection.toString()) {
        resolve();
        return;
      }

      this.toolsActivated = selection && selection.toString() && selection.toString().length > 0;

      if (this.toolsActivated) {
        this.selection = selection;

        const tools: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-tools');

        if (tools && selection.rangeCount > 0) {
          let top: number = this.unifyEvent($event).clientY;
          let left: number = this.unifyEvent($event).clientX;

          if (this.toolbarOffsetHeight > 0) {
            top = top - this.toolbarOffsetHeight;
          }

          tools.style.top = '' + (top) + 'px';
          tools.style.left = '' + (left) + 'px';
        }
      }

      resolve();
    });
  }

  private getSelection(): Promise<Selection> {
    return new Promise<Selection>((resolve) => {
      let selectedSelection: Selection = null;

      if (window && window.getSelection) {
        selectedSelection = window.getSelection();
      } else if (document && document.getSelection) {
        selectedSelection = document.getSelection();
      } else if (document && (document as any).selection) {
        selectedSelection = (document as any).selection.createRange().text;
      }

      resolve(selectedSelection);
    });
  }

  private clearTheSelection(): Promise<Selection> {
    return new Promise<Selection>((resolve) => {
      if (window && window.getSelection) {
        if (window.getSelection().empty) {
          window.getSelection().empty();
        } else if (window.getSelection().removeAllRanges) {
          window.getSelection().removeAllRanges();
        }
      } else if (document && (document as any).selection) {
        (document as any).selection.empty();
      }

      resolve();
    });
  }

  // Touch or Mouse
  private unifyEvent(e: any): any {
    return e.changedTouches ? e.changedTouches[0] : e;
  }

  private toggle(e: UIEvent, type: ContentType): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      if (!document) {
        resolve();
        return;
      }

      if (!this.selection || !this.selection.anchorNode || this.selection.rangeCount <= 0) {
        resolve();
        return;
      }

      const range: Range = this.selection.getRangeAt(0);

      let content: Node = range.commonAncestorContainer ? range.commonAncestorContainer : this.selection.anchorNode;

      if (!content || !content.parentElement) {
        resolve();
        return;
      }

      // TODO: Find a clever way to detect to root container
      // In case a part of the container has been set to bold, italic etc.
      if (content.parentElement.nodeName.toLowerCase() === 'span' || content.parentElement.nodeName.toLowerCase() === 'b') {
        content = content.parentElement;
      }

      const container: HTMLElement = content.parentElement;

      if (this.type !== type) {
        const element: HTMLElement = document.createElement(type.toString());

        if (container.attributes && container.attributes.length) {
          for (let i: number = 0; i<container.attributes.length; i++) {
            element.setAttribute(container.attributes[i].name, container.attributes[i].value);
          }
        }

        if (container.childNodes && container.childNodes.length > 0) {
          const elements: HTMLElement[] = Array.prototype.slice.call(container.childNodes);
          elements.forEach((e: HTMLElement) => {
            element.appendChild(e);
          })
        }

        container.parentElement.replaceChild(element, container);

        this.type = type;
      }

      await this.reset(true);

      resolve();
    });
  }

  private async reset(clearSelection: boolean) {
    if (clearSelection) {
      await this.clearTheSelection();
    }

    this.toolsActivated = false;
    this.selection = null;
  }

  private bold(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      if (!this.selection || this.selection.rangeCount <= 0) {
        resolve();
        return;
      }

      const text: string = this.selection.toString();

      if (!text || text.length <= 0) {
        resolve();
        return;
      }

      document.execCommand('bold');

      resolve();
    });
  }

  // TODO: italic with em

  render() {
    return (<div class={this.toolsActivated ? "deckgo-tools deckgo-tools-activated" : "deckgo-tools"}>
      <button onClick={(e: UIEvent) => this.bold(e)}>Bold</button>
      <button onClick={(e: UIEvent) => this.toggle(e, ContentType.H1)}>H1</button>
      <button onClick={(e: UIEvent) => this.toggle(e, ContentType.H2)}>H2</button>
      <button onClick={(e: UIEvent) => this.toggle(e, ContentType.H3)}>H3</button>
    </div>);
  }

}
