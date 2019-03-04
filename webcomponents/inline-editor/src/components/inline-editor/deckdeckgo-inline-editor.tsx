import {Component, Element, Listen, Prop, State} from '@stencil/core';

import {ContentType} from '../../types/inline-editor/deckdeckgo-inline-editor-types';

@Component({
  tag: 'deckgo-inline-editor',
  styleUrl: 'deckdeckgo-inline-editor.scss',
  shadow: true
})
export class DeckdeckgoInlineEditor {

  @Element() el: HTMLElement;

  @State()
  private type: ContentType;

  @Prop()
  toolbarOffsetHeight: number;

  @State()
  private toolsActivated: boolean = false;

  private selection: Selection = null;

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

      this.toolsActivated = await this.activateToolbar(selection);

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

  private activateToolbar(selection: Selection): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const tools: boolean = selection && selection.toString() && selection.toString().length > 0;

      if (tools) {
        await this.initContentType(selection);
      }

      resolve(tools);
    });
  }

  private initContentType(selection: Selection): Promise<void> {
    return new Promise<void>(async (resolve) => {

      console.log('here');

      if (!selection || selection.rangeCount <= 0) {
        resolve();
        return;
      }

      const range: Range = selection.getRangeAt(0);

      const content: Node = range.commonAncestorContainer ? range.commonAncestorContainer : this.selection.anchorNode;

      if (!content) {
        resolve();
        return;
      }

      const container: HTMLElement = await this.getContainer(content);

      this.type = ContentType[container.nodeName.toUpperCase()];

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

      if (!this.selection || this.selection.rangeCount <= 0) {
        resolve();
        return;
      }

      const range: Range = this.selection.getRangeAt(0);

      let content: Node = range.commonAncestorContainer ? range.commonAncestorContainer : this.selection.anchorNode;

      if (!content) {
        resolve();
        return;
      }

      const container: HTMLElement = await this.getContainer(content);

      // If click again, default is a paragraph
      type = this.type === type ? ContentType.P : type;

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

  // TODO: Find a clever way to detect to root container
  // We iterate until we find the root container which should be one of the supported content type
  private getContainer(presumedTopLevelNode: Node): Promise<HTMLElement> {
    return new Promise<HTMLElement>(async (resolve) => {
      if (!presumedTopLevelNode) {
        resolve(null);
        return;
      }

      if (ContentType[presumedTopLevelNode.nodeName.toUpperCase()]) {
        resolve(presumedTopLevelNode as HTMLElement);
      } else {
        const parentElement: HTMLElement = await this.getContainer(presumedTopLevelNode.parentNode);

        resolve(parentElement);
      }
    });
  }

  private async reset(clearSelection: boolean) {
    if (clearSelection) {
      await this.clearTheSelection();
    }

    this.toolsActivated = false;
    this.selection = null;
    this.type = null;
  }

  private bold(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await this.applyStyle('bold');

      resolve();
    });
  }

  private italic(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await this.applyStyle('italic');

      resolve();
    });
  }

  private underline(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await this.applyStyle('underline');

      resolve();
    });
  }

  private applyStyle(style: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selection || this.selection.rangeCount <= 0 || !document) {
        resolve();
        return;
      }

      const text: string = this.selection.toString();

      if (!text || text.length <= 0) {
        resolve();
        return;
      }

      document.execCommand(style);

      resolve();
    });
  }

  // TODO: button unclick
  // TODO: link

  render() {
    return (<div class={this.toolsActivated ? "deckgo-tools deckgo-tools-activated" : "deckgo-tools"}>
      <button onClick={(e: UIEvent) => this.bold(e)} class="bold">B</button>
      <button onClick={(e: UIEvent) => this.italic(e)} class="italic">I</button>
      <button onClick={(e: UIEvent) => this.underline(e)} class="underline">U</button>

      <div class="separator"></div>

      <button onClick={(e: UIEvent) => this.toggle(e, ContentType.H1)} class={this.type === ContentType.H1 ? "h1 active" : "h1"}>T</button>
      <button onClick={(e: UIEvent) => this.toggle(e, ContentType.H2)} class={this.type === ContentType.H2 ? "h2 active" : "h2"}>T</button>
      <button onClick={(e: UIEvent) => this.toggle(e, ContentType.H3)} class={this.type === ContentType.H3 ? "h3 active" : "h3"}>T</button>
    </div>);
  }

}
