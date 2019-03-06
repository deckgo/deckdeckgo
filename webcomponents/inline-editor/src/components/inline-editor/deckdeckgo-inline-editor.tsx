import {Component, Element, Listen, Prop, State} from '@stencil/core';

import {DeckdeckgoInlineEditorTag} from '../../types/inline-editor/deckdeckgo-inline-editor-tag';
import {DeckdeckgoInlineEditorUtils} from '../../types/inline-editor/deckdeckgo-inline-editor-utils';

@Component({
  tag: 'deckgo-inline-editor',
  styleUrl: 'deckdeckgo-inline-editor.scss',
  shadow: true
})
export class DeckdeckgoInlineEditor {

  @Element() el: HTMLElement;

  @State()
  private type: DeckdeckgoInlineEditorTag;

  @State()
  private bold: boolean = false;

  @State()
  private italic: boolean = false;

  @State()
  private underline: boolean = false;

  @State()
  private styledElements: boolean = false;

  @Prop({mutable: true})
  mobile: boolean = false;

  @Prop()
  toolbarOffsetHeight: number;

  @Prop()
  sticky: boolean = false;

  @State()
  private toolsActivated: boolean = false;

  private selection: Selection = null;
  private anchorEvent: MouseEvent | TouchEvent;

  componentDidLoad() {
    if (!this.mobile) {
      this.mobile = DeckdeckgoInlineEditorUtils.isMobile();
    }
  }

  @Listen('document:mousedown', {passive: true})
  async mousedown($event: MouseEvent) {
    this.anchorEvent = $event;
  }

  @Listen('document:touchstart', {passive: true})
  async touchstart($event: MouseEvent) {
    this.anchorEvent = $event;
  }

  @Listen('document:selectionchange', {passive: true})
  async selectionchange(_$event: Event) {
    await this.displayTools();
  }

  @Listen('document:keydown', {passive: true})
  async keydown($event: KeyboardEvent) {
    if ($event && ($event.key.toLowerCase() === 'backspace' || $event.key.toLowerCase() === 'delete')) {
      await this.reset(false);
    }
  }

  private displayTools(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const selection: Selection = await this.getSelection();

      if (!this.anchorEvent) {
        await this.reset(false);
        resolve();
        return;
      }

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

        await this.setToolbarAnchorPosition(selection);
      }

      resolve();
    });
  }

  private setToolbarAnchorPosition(selection: Selection): Promise<void> {
    return new Promise<void>((resolve) => {
      if (this.sticky) {
        resolve();
        return;
      }

      const tools: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-tools');

      if (tools && selection.rangeCount > 0) {
        let top: number = this.unifyEvent(this.anchorEvent).clientY;
        let left: number = this.unifyEvent(this.anchorEvent).clientX;

        if (this.toolbarOffsetHeight > 0) {
          top = top - this.toolbarOffsetHeight;
        }

        if (this.mobile) {
          top = top + 40;
        }

        const innerWidth: number = DeckdeckgoInlineEditorUtils.isIOS() ? screen.width : window.innerWidth;

        if (innerWidth > 0 && left > innerWidth - 250) {
          left = innerWidth - 250;
        }

        tools.style.top = '' + (top) + 'px';
        tools.style.left = '' + (left) + 'px';
      }

      resolve();
    });
  }

  private activateToolbar(selection: Selection): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const tools: boolean = selection && selection.toString() && selection.toString().length > 0;

      if (tools) {
        const promises = [];

        promises.push(this.initContentType(selection));
        promises.push(this.initStyle(selection));

        await Promise.all(promises);
      }

      resolve(tools);
    });
  }

  private initContentType(selection: Selection): Promise<void> {
    return new Promise<void>(async (resolve) => {
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

      this.type = DeckdeckgoInlineEditorTag[container.nodeName.toUpperCase()];

      resolve();
    });
  }


  private initStyle(selection: Selection): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!selection || selection.rangeCount <= 0) {
        resolve();
        return;
      }

      const content: Node = selection.anchorNode;

      if (!content) {
        resolve();
        return;
      }

      // It happens on mobile devices
      const parentDiv: boolean = content.parentElement && content.parentElement.nodeName && content.parentElement.nodeName.toLowerCase() === 'div';

      if (DeckdeckgoInlineEditorTag[content.nodeName.toUpperCase()] || parentDiv) {
        this.bold = false;
        this.italic = false;
        this.underline = false;

        await this.findStyle(content);
      } else if (content.parentElement) {
        this.bold = false;
        this.italic = false;
        this.underline = false;

        await this.findStyle(content.parentElement);
      }

      resolve();
    });
  }

  // TODO: Find a clever way to detect to root container
  // We iterate until we find the root container to detect if bold, underline or italic are active
  private findStyle(node: Node): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!node) {
        resolve();
        return;
      }

      // Just in case
      if (node.nodeName.toUpperCase() === 'HTML' || node.nodeName.toUpperCase() === 'BODY' || node.nodeName.toUpperCase() === 'DIV') {
        resolve();
        return;
      }

      if (DeckdeckgoInlineEditorTag[node.nodeName.toUpperCase()]) {
        const children: HTMLCollection = (node as HTMLElement).children;
        this.styledElements = children && children.length > 0;
        resolve();
      } else {
        this.bold = await DeckdeckgoInlineEditorUtils.isBold((node as HTMLElement));
        this.italic = await DeckdeckgoInlineEditorUtils.isItalic((node as HTMLElement));
        this.underline = await DeckdeckgoInlineEditorUtils.isUnderline((node as HTMLElement));

        await this.findStyle(node.parentNode);

        resolve();
      }
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

  private toggle(e: UIEvent, type: DeckdeckgoInlineEditorTag): Promise<void> {
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
      type = this.type === type ? DeckdeckgoInlineEditorTag.P : type;

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

      if (DeckdeckgoInlineEditorTag[presumedTopLevelNode.nodeName.toUpperCase()]) {
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

  private styleBold(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await this.applyStyle('bold');

      await this.initStyle(this.selection);

      resolve();
    });
  }

  private styleItalic(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await this.applyStyle('italic');

      await this.initStyle(this.selection);

      resolve();
    });
  }

  private styleUnderline(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await this.applyStyle('underline');

      await this.initStyle(this.selection);

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

  // TODO: link

  render() {
    let classNames: string = this.toolsActivated ? (this.mobile ? 'deckgo-tools deckgo-tools-activated deckgo-tools-mobile' : 'deckgo-tools deckgo-tools-activated') : (this.mobile ? 'deckgo-tools deckgo-tools-mobile' : 'deckgo-tools');

    if (this.sticky) {
      classNames += ' deckgo-tools-sticky';
    }

    return (<div class={classNames}>
      <button onClick={(e: UIEvent) => this.styleBold(e)} disabled={this.type !== undefined && this.type !== DeckdeckgoInlineEditorTag.P} class={this.bold ? "bold active" : "bold"}>B</button>
      <button onClick={(e: UIEvent) => this.styleItalic(e)} disabled={this.type !== undefined && this.type !== DeckdeckgoInlineEditorTag.P} class={this.italic ? "italic active" : "italic"}>I</button>
      <button onClick={(e: UIEvent) => this.styleUnderline(e)} disabled={this.type !== undefined && this.type !== DeckdeckgoInlineEditorTag.P} class={this.underline ? "underline active" : "underline"}>U</button>

      <div class="separator"></div>

      <button onClick={(e: UIEvent) => this.toggle(e, DeckdeckgoInlineEditorTag.H1)} disabled={this.bold || this.italic || this.underline || this.styledElements} class={this.type === DeckdeckgoInlineEditorTag.H1 ? "h1 active" : "h1"}>T</button>
      <button onClick={(e: UIEvent) => this.toggle(e, DeckdeckgoInlineEditorTag.H2)} disabled={this.bold || this.italic || this.underline || this.styledElements} class={this.type === DeckdeckgoInlineEditorTag.H2 ? "h2 active" : "h2"}>T</button>
      <button onClick={(e: UIEvent) => this.toggle(e, DeckdeckgoInlineEditorTag.H3)} disabled={this.bold || this.italic || this.underline || this.styledElements} class={this.type === DeckdeckgoInlineEditorTag.H3 ? "h3 active" : "h3"}>T</button>
    </div>);
  }

}
