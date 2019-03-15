import {Component, Element, Listen, Prop, State} from '@stencil/core';

import {DeckdeckgoInlineEditorTag} from '../../types/inline-editor/deckdeckgo-inline-editor-tag';
import {DeckdeckgoInlineEditorUtils} from '../../types/inline-editor/deckdeckgo-inline-editor-utils';

interface AnchorLink {
  range: Range;
  text: string;
}

interface InputTargetEvent extends EventTarget {
  value: string;
}

@Component({
  tag: 'deckgo-inline-editor',
  styleUrl: 'deckdeckgo-inline-editor.scss',
  shadow: true
})
export class DeckdeckgoInlineEditor {

  @Element() el: HTMLElement;

  @State()
  private bold: boolean = false;

  @State()
  private italic: boolean = false;

  @State()
  private underline: boolean = false;

  @State()
  private color: string;

  @State()
  private disableBold: boolean = false;

  @Prop({mutable: true})
  mobile: boolean = false;

  @Prop()
  stickyDesktop: boolean = false;

  @Prop()
  stickyMobile: boolean = false;

  @State()
  private toolsActivated: boolean = false;

  private selection: Selection = null;

  private anchorLink: AnchorLink = null;
  private anchorEvent: MouseEvent | TouchEvent;

  @State()
  private link: boolean = false;

  @State()
  private linkInput: boolean = false;

  private linkUrl: string;

  async componentDidLoad() {
    await this.colorPickerListener(true);

    if (!this.mobile) {
      this.mobile = DeckdeckgoInlineEditorUtils.isMobile();
    }
  }

  async componentDidUnload() {
    await this.colorPickerListener(false);
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
    if (this.linkInput && document && document.activeElement && document.activeElement.nodeName && document.activeElement.nodeName.toLowerCase() === 'deckgo-inline-editor') {
      return;
    }

    await this.displayTools();
  }

  @Listen('document:keydown', {passive: true})
  async keydown($event: KeyboardEvent) {
    if (!$event) {
      return;
    }

    if (!this.linkInput && ($event.key.toLowerCase() === 'backspace' || $event.key.toLowerCase() === 'delete')) {
      await this.reset(false);
    } else if (this.linkInput && $event.key.toLowerCase() === 'enter') {
      await this.createLink();
      await this.reset(true);
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

        if (selection.rangeCount > 0) {
          const range: Range = selection.getRangeAt(0);
          this.anchorLink = {
            range: range,
            text: selection.toString()
          };
        }

        await this.setToolbarAnchorPosition(selection);
      }

      resolve();
    });
  }

  private setToolbarAnchorPosition(selection: Selection): Promise<void> {
    return new Promise<void>((resolve) => {
      if (this.isSticky()) {
        resolve();
        return;
      }

      const tools: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-tools');

      if (tools && selection.rangeCount > 0) {
        let top: number = this.unifyEvent(this.anchorEvent).clientY;
        let left: number = this.unifyEvent(this.anchorEvent).clientX;

        if (this.mobile) {
          top = top + 40;
        }

        const innerWidth: number = DeckdeckgoInlineEditorUtils.isIOS() ? screen.width : window.innerWidth;

        if (innerWidth > 0 && left > innerWidth - 300) {
          left = innerWidth - 300;
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

        promises.push(this.initStyle(selection));
        promises.push(this.initLink(selection));

        await Promise.all(promises);
      }

      resolve(tools);
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
        this.color = null;

        await this.findStyle(content);
      } else if (content.parentElement) {
        this.bold = false;
        this.italic = false;
        this.underline = false;
        this.color = null;

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
        this.disableBold = DeckdeckgoInlineEditorTag[node.nodeName.toUpperCase()] !== DeckdeckgoInlineEditorTag.P;

        await this.findColor(node);

        resolve();
      } else {
        this.bold = await DeckdeckgoInlineEditorUtils.isBold((node as HTMLElement));
        this.italic = await DeckdeckgoInlineEditorUtils.isItalic((node as HTMLElement));
        this.underline = await DeckdeckgoInlineEditorUtils.isUnderline((node as HTMLElement));

        await this.findColor(node);

        await this.findStyle(node.parentNode);

        resolve();
      }
    });
  }

  private findColor(node: Node): Promise<void> {
    return new Promise<void>((resolve) => {
      if (this.color && this.color !== '') {
        resolve();
        return;
      }

      if ((node as HTMLElement).style.color) {
        this.color = (node as HTMLElement).style.color;
      } else if (node instanceof HTMLFontElement && (node as HTMLFontElement).color) {
        this.color = (node as HTMLFontElement).color;
      }

      resolve();
    });
  }

  private initLink(selection: Selection): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!selection) {
        resolve();
        return;
      }

      let content: Node = selection.anchorNode;

      if (!content) {
        resolve();
        return;
      }

      if (content.nodeType === 3) {
        content = content.parentElement;
      }

      this.link = content.nodeName && content.nodeName.toLowerCase() === 'a';

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

  private async reset(clearSelection: boolean) {
    if (clearSelection) {
      await this.clearTheSelection();
    }

    this.toolsActivated = false;
    this.selection = null;

    this.linkInput = false;
    this.anchorLink = null;
    this.link = false;
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

  private toggleLink(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.link) {
        await this.removeLink();
        await this.reset(true);
      } else {
        await this.openLink();
      }

      resolve();
    });
  }

  private removeLink(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selection) {
        resolve();
        return;
      }

      let content: Node = this.selection.anchorNode;

      if (!content || !content.parentElement) {
        resolve();
        return;
      }

      if (content.nodeType === 3) {
        content = content.parentElement;
      }

      if (!content.nodeName && content.nodeName.toLowerCase() !== 'a') {
        resolve();
        return;
      }

      content.parentElement.insertBefore(document.createTextNode(content.textContent), content);
      content.parentElement.removeChild(content);

      resolve();
    });
  }

  private openLink(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.linkInput = true;

      resolve();
    });
  }

  private createLink(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!document) {
        resolve();
        return;
      }

      if (!this.anchorLink) {
        resolve();
        return;
      }

      if (!this.linkUrl || this.linkUrl.length <= 0) {
        resolve();
        return;
      }

      let container: Node = this.anchorLink.range.commonAncestorContainer ? this.anchorLink.range.commonAncestorContainer : this.selection.anchorNode;

      if (!container) {
        resolve();
        return;
      }

      // If node text
      if (container.nodeType === 3) {
        container = container.parentElement;
      }

      const target: Node = Array.from(container.childNodes).find((node: Node) => {
        return node.textContent && node.textContent.trim().indexOf(this.anchorLink.text) > -1;
      });

      if (!target) {
        resolve();
        return;
      }

      if (target.nodeType === 3) {
        const index: number = target.textContent.indexOf(this.anchorLink.text);

        const textBefore: string = index > -1 ? target.textContent.substr(0, index) : null;
        const textAfter: string = (index + this.anchorLink.text.length) > -1 ? target.textContent.substr((index + this.anchorLink.text.length)) : null;

        if (textBefore) {
          target.parentElement.appendChild(document.createTextNode(textBefore));
        }

        const a: HTMLAnchorElement = await this.createLinkElement();
        target.parentElement.appendChild(a);

        if (textAfter) {
          target.parentElement.appendChild(document.createTextNode(textAfter));
        }

        target.parentElement.removeChild(target);

      } else {
        const a: HTMLAnchorElement = await this.createLinkElement();

        target.parentElement.replaceChild(a, target);
      }

      this.linkInput = false;

      resolve();
    });
  }

  private createLinkElement(): Promise<HTMLAnchorElement> {
    return new Promise<HTMLAnchorElement>((resolve) => {
      const a: HTMLAnchorElement = document.createElement('a');
      const linkText: Text = document.createTextNode(this.anchorLink.text);
      a.appendChild(linkText);
      a.title = this.anchorLink.text;
      a.href = this.linkUrl;

      resolve(a);
    });
  }

  private handleLinkInput($event: UIEvent) {
    this.linkUrl = ($event.target as InputTargetEvent).value;
  }

  private isSticky(): boolean {
    const mobile: boolean = DeckdeckgoInlineEditorUtils.isMobile();

    // On iOS, when the keyboard opens, it doesn't resize the window/viewport, therefore be my guest to set the toolbar as sticky footer without any other requirements
    return (this.stickyDesktop && !mobile) || (this.stickyMobile && mobile && !DeckdeckgoInlineEditorUtils.isIOS());
  }

  // Color picker

  private colorPickerListener(bind: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      const colorPicker: HTMLInputElement = this.el.shadowRoot.querySelector('input[name=\'color-picker\']');

      if (!colorPicker) {
        resolve();
        return;
      }

      if (bind) {
        colorPicker.addEventListener('change', this.selectColor, false);
      } else {
        colorPicker.removeEventListener('change', this.selectColor, true);
      }


      resolve();
    });
  }

  private selectColor = async ($event) => {
    if (!this.selection) {
      return;
    }

    this.color = $event.target.value;

    if (!this.selection || this.selection.rangeCount <= 0 || !document) {
      return;
    }

    const text: string = this.selection.toString();

    if (!text || text.length <= 0) {
      return;
    }

    document.execCommand('foreColor', false, this.color);
  };

  private openColorPicker(): Promise<void> {
    return new Promise<void>((resolve) => {
      const colorPicker: HTMLInputElement = this.el.shadowRoot.querySelector('input[name=\'color-picker\']');

      if (!colorPicker) {
        resolve();
        return;
      }

      colorPicker.click();

      this.toolsActivated = false;

      resolve();
    });
  }

  render() {
    let classNames: string = this.toolsActivated ? (this.mobile ? 'deckgo-tools deckgo-tools-activated deckgo-tools-mobile' : 'deckgo-tools deckgo-tools-activated') : (this.mobile ? 'deckgo-tools deckgo-tools-mobile' : 'deckgo-tools');

    if (this.isSticky()) {
      classNames += ' deckgo-tools-sticky';
    }

    return <div class={classNames}>
      {this.renderActions()}
      <input type="color" name="color-picker" value={this.color}></input>
    </div>;
  }

  private renderActions() {
    if (this.linkInput) {
      return (
        <div class="link">
          <input autofocus placeholder="Add a link..." onInput={($event: UIEvent) => this.handleLinkInput($event)}></input>
        </div>
      );
    } else {
      const styleColor = this.color ? {'border-bottom': '2px solid ' + this.color} : {};

      return [
        <button onClick={(e: UIEvent) => this.styleBold(e)} disabled={this.disableBold}
                class={this.bold ? "bold active" : "bold"}>B
        </button>,
        <button onClick={(e: UIEvent) => this.styleItalic(e)}
                class={this.italic ? "italic active" : "italic"}>I
        </button>,
        <button onClick={(e: UIEvent) => this.styleUnderline(e)}
                class={this.underline ? "underline active" : "underline"}>
            <span>U</span>
        </button>,

        <div class="separator"></div>,

        <button onClick={() => this.openColorPicker()} class="color">
            <span style={styleColor}>A</span>
        </button>,

        <div class="separator"></div>,

        <button onClick={() => {this.toggleLink()}} class={this.link ? "link active" : "link"}>
          <slot name="link"></slot>
        </button>
      ];
    }
  }
}
