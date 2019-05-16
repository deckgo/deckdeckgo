import {Component, Element, EventEmitter, Listen, Prop, State, Watch, Event} from '@stencil/core';

import {DeckdeckgoInlineEditorUtils} from '../../types/inline-editor/deckdeckgo-inline-editor-utils';

interface AnchorLink {
  range: Range;
  text: string;
}

interface InputTargetEvent extends EventTarget {
  value: string;
}

enum ToolbarActions {
  SELECTION,
  LINK,
  IMAGE
}

enum ImageSize {
  SMALL = '25%',
  MEDIUM = '50%',
  LARGE = '75%',
  ORIGINAL = '100%'
}

enum ImageAlign {
  STANDARD,
  START
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
  private orderedList: boolean = false;

  @State()
  private unorderedList: boolean = false;

  @State()
  private imageSize: ImageSize;

  @State()
  private imageAlign: ImageAlign;

  @State()
  private color: string;

  @State()
  private disabledTitle: boolean = false;

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
  private toolbarActions: ToolbarActions = ToolbarActions.SELECTION;

  private linkUrl: string;

  @Prop()
  attachTo: HTMLElement;

  @Prop()
  containers: string = 'h1,h2,h3,h4,h5,h6,div';

  @Event() private imgDidChange: EventEmitter<HTMLElement>;

  @Prop()
  imgAnchor: string = 'img';

  @Prop()
  imgPropertyWidth: string = 'width';

  @Prop()
  imgPropertyCssFloat: string = 'cssFloat';

  async componentWillLoad() {
    await this.attachListener();
  }

  async componentDidLoad() {
    await this.colorPickerListener(true);

    if (!this.mobile) {
      this.mobile = DeckdeckgoInlineEditorUtils.isMobile();
    }
  }

  async componentDidUnload() {
    await this.colorPickerListener(false);

    await this.detachListener(this.attachTo ? this.attachTo : document);
  }

  @Watch('attachTo')
  async onAttachTo() {
    if (!this.attachTo) {
      return;
    }

    await this.detachListener(document);
    await this.attachListener();
  }

  private attachListener(): Promise<void> {
    return new Promise<void>((resolve) => {
      const listenerElement: HTMLElement | Document = this.attachTo ? this.attachTo : document;
      if (listenerElement) {
        listenerElement.addEventListener('mousedown', this.mousedown, {passive: true});
        listenerElement.addEventListener('touchstart', this.touchstart, {passive: true});
      }

      resolve();
    });
  }

  private detachListener(listenerElement: HTMLElement | Document): Promise<void> {
    return new Promise<void>((resolve) => {
      if (listenerElement) {
        listenerElement.removeEventListener('mousedown', this.mousedown);
        listenerElement.removeEventListener('touchstart', this.touchstart);
      }

      resolve();
    });
  }

  private mousedown = async ($event: MouseEvent) => {
    if (this.toolsActivated) {
      return;
    }

    this.anchorEvent = $event;

    await this.displayImageActions();
  };

  private touchstart = async ($event: MouseEvent) => {
    if (this.toolsActivated) {
      return;
    }

    this.anchorEvent = $event;

    await this.displayImageActions();
  };

  private displayImageActions(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const isAnchorImg: boolean = await this.isAnchorImage();
      if (!isAnchorImg) {
        resolve();
        return;
      }

      await this.reset(true);

      setTimeout(async () => {
        await this.activateToolbarImage();
        await this.setToolbarAnchorPosition();
      }, 0);

      resolve();
    });
  }

  private activateToolbarImage(): Promise<void> {
    return new Promise<void>((resolve) => {
      const target: HTMLImageElement = this.anchorEvent.target as HTMLImageElement;

      if (target.style.getPropertyValue(this.imgPropertyWidth) === '25%') {
        this.imageSize = ImageSize.SMALL;
      } else if (target.style.getPropertyValue(this.imgPropertyWidth) === '50%') {
        this.imageSize = ImageSize.MEDIUM;
      } else if (target.style.getPropertyValue(this.imgPropertyWidth) === '75%') {
        this.imageSize = ImageSize.LARGE;
      } else {
        this.imageSize = ImageSize.ORIGINAL;
      }

      if (target.style.getPropertyValue(this.imgPropertyCssFloat) === 'left') {
        this.imageAlign = ImageAlign.START;
      } else {
        this.imageAlign = ImageAlign.STANDARD;
      }

      this.toolbarActions = ToolbarActions.IMAGE;
      this.toolsActivated = true;

      resolve();
    });
  }

  private isAnchorImage(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      if (!this.anchorEvent) {
        resolve(false);
        return;
      }

      if (!this.anchorEvent.target || !(this.anchorEvent.target instanceof HTMLElement)) {
        resolve(false);
        return;
      }

      const target: HTMLElement = this.anchorEvent.target;

      resolve(target.nodeName && target.nodeName.toLowerCase() === this.imgAnchor);
    });
  }

  @Listen('document:selectionchange', {passive: true})
  async selectionchange(_$event: Event) {
    if (document && document.activeElement && !this.isContainer(document.activeElement)) {
      if (document.activeElement.nodeName.toLowerCase() !== 'deckgo-inline-editor') {
        await this.reset(false);
      }

      return;
    }

    await this.displayTools();
  }

  private displayTools(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const selection: Selection = await this.getSelection();

      if (!this.anchorEvent) {
        await this.reset(false);
        resolve();
        return;
      }

      if (this.attachTo && !this.attachTo.contains((this.anchorEvent.target as Node))) {
        await this.reset(false);
        resolve();
        return;
      }

      if (!selection || !selection.toString() || selection.toString().trim().length <= 0) {
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

          await this.setToolbarAnchorPosition();
        }
      }

      resolve();
    });
  }

  private setToolbarAnchorPosition(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (this.isSticky()) {
        resolve();
        return;
      }

      const tools: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-tools');

      if (tools) {
        let top: number = this.unifyEvent(this.anchorEvent).clientY;
        let left: number = this.unifyEvent(this.anchorEvent).clientX;

        if (this.mobile) {
          top = top + 40;
        } else {
          top = top + 10;
        }

        const innerWidth: number = DeckdeckgoInlineEditorUtils.isIOS() ? screen.width : window.innerWidth;

        if (innerWidth > 0 && left > innerWidth - 340) {
          left = innerWidth - 340;
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

      if (this.isContainer(content)) {
        this.bold = false;
        this.italic = false;
        this.underline = false;
        this.orderedList = false;
        this.unorderedList = false;
        this.color = null;

        await this.findStyle(content);
      } else if (content.parentElement) {
        this.bold = false;
        this.italic = false;
        this.underline = false;
        this.orderedList = false;
        this.unorderedList = false;
        this.color = null;

        await this.findStyle(content.parentElement);
      }

      resolve();
    });
  }

  private isContainer(element: Node): boolean {
    const containerTypes: string[] = this.containers.toLowerCase().split(',');
    return element && element.nodeName && containerTypes.indexOf(element.nodeName.toLowerCase()) > -1;
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
      if (node.nodeName.toUpperCase() === 'HTML' || node.nodeName.toUpperCase() === 'BODY') {
        resolve();
        return;
      }

      if (this.isContainer(node)) {
        const nodeName: string = node.nodeName.toUpperCase();

        this.disabledTitle = nodeName === 'H1' || nodeName === 'H2' || nodeName === 'H3' || nodeName === 'H4' || nodeName === 'H5' || nodeName === 'H6';

        await this.findColor(node);

        resolve();
      } else {
        this.bold = await DeckdeckgoInlineEditorUtils.isBold((node as HTMLElement));
        this.italic = await DeckdeckgoInlineEditorUtils.isItalic((node as HTMLElement));
        this.underline = await DeckdeckgoInlineEditorUtils.isUnderline((node as HTMLElement));

        if (!this.orderedList) {
          this.orderedList = await DeckdeckgoInlineEditorUtils.isList((node as HTMLElement), 'ol');
        }

        if (!this.unorderedList) {
          this.unorderedList = await DeckdeckgoInlineEditorUtils.isList((node as HTMLElement), 'ul');
        }

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

    this.toolbarActions = ToolbarActions.SELECTION;
    this.anchorLink = null;
    this.link = false;
  }

  private styleBold(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await this.execCommand('bold');

      await this.initStyle(this.selection);

      resolve();
    });
  }

  private styleItalic(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await this.execCommand('italic');

      await this.initStyle(this.selection);

      resolve();
    });
  }

  private styleUnderline(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await this.execCommand('underline');

      await this.initStyle(this.selection);

      resolve();
    });
  }

  private toggleList(e: UIEvent, cmd: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await this.execCommand(cmd);

      await this.reset(true);

      resolve();
    });
  }

  private execCommand(command: string): Promise<void> {
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

      document.execCommand(command);

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
      this.toolbarActions = ToolbarActions.LINK;

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

      this.toolbarActions = ToolbarActions.SELECTION;

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

  private async handleLinkEnter($event: KeyboardEvent) {
    if (!$event) {
      return;
    }

    if (this.toolbarActions === ToolbarActions.SELECTION && ($event.key.toLowerCase() === 'backspace' || $event.key.toLowerCase() === 'delete')) {
      await this.reset(false);
    } else if (this.toolbarActions === ToolbarActions.LINK && $event.key.toLowerCase() === 'enter') {
      await this.createLink();
      await this.reset(true);
    }
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

  private styleImage(e: UIEvent, applyFunction: Function, param: ImageSize | ImageAlign): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const isAnchorImg: boolean = await this.isAnchorImage();
      if (!isAnchorImg) {
        resolve();
        return;
      }

      e.stopPropagation();

      applyFunction(param);

      const anchorImg: HTMLImageElement = this.anchorEvent.target as HTMLImageElement;
      const container: HTMLElement = await this.findContainer(anchorImg);
      this.imgDidChange.emit(container);

      await this.reset(true);

      resolve();
    });
  }

  private setImageWith = async (size: ImageSize) => {
    const anchorImg: HTMLImageElement = this.anchorEvent.target as HTMLImageElement;
    anchorImg.style.setProperty(this.imgPropertyWidth, size.toString());
  };

  private setImageAlignment = (align: ImageAlign) => {
    const anchorImg: HTMLImageElement = this.anchorEvent.target as HTMLImageElement;

    if (align === ImageAlign.START) {
      anchorImg.style.setProperty(this.imgPropertyCssFloat, 'left');
    } else {
      anchorImg.style.removeProperty(this.imgPropertyCssFloat);
    }
  };

  private findContainer(element: HTMLElement): Promise<HTMLElement> {
    return new Promise<HTMLElement>(async (resolve) => {
      if (!element) {
        resolve();
        return;
      }

      // Just in case
      if (element.nodeName.toUpperCase() === 'HTML' || element.nodeName.toUpperCase() === 'BODY' || !element.parentElement) {
        resolve(element);
        return;
      }

      if (this.isContainer(element)) {
        resolve(element);
      } else {
        const container: HTMLElement = await this.findContainer(element.parentElement);

        resolve(container);
      }
    });
  }

  private deleteImage(e: UIEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const isAnchorImg: boolean = await this.isAnchorImage();
      if (!isAnchorImg) {
        resolve();
        return;
      }

      e.stopPropagation();

      const anchorImg: HTMLImageElement = this.anchorEvent.target as HTMLImageElement;

      if (!anchorImg || !anchorImg.parentElement) {
        resolve();
        return;
      }

      const container: HTMLElement = await this.findContainer(anchorImg);

      if (!container) {
        resolve();
        return;
      }

      anchorImg.parentElement.removeChild(anchorImg);

      this.imgDidChange.emit(container);

      await this.reset(true);

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
    if (this.toolbarActions === ToolbarActions.LINK) {
      return (
        <div class="link">
          <input autofocus placeholder="Add a link..."
                 onInput={($event: UIEvent) => this.handleLinkInput($event)}
                 onKeyUp={($event: KeyboardEvent) => this.handleLinkEnter($event)}
          ></input>
        </div>
      );
    } else if (this.toolbarActions === ToolbarActions.IMAGE) {
      return this.renderImageActions();
    } else {
      return this.renderSelectionActions();
    }
  }

  private renderSelectionActions() {
    const styleColor = this.color ? {'border-bottom': '2px solid ' + this.color} : {};

    return [
      <button onClick={(e: UIEvent) => this.styleBold(e)} disabled={this.disabledTitle}
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

      <button
        disabled={this.disabledTitle}
        onClick={(e: UIEvent) => this.toggleList(e, 'insertOrderedList')}
        class={this.orderedList ? "ordered-list active" : "ordered-list"}>
        <div></div>
      </button>,

      <button
        disabled={this.disabledTitle}
        onClick={(e: UIEvent) => this.toggleList(e, 'insertUnorderedList')}
        class={this.unorderedList ? "unordered-list active" : "unordered-list"}>
        <div></div>
      </button>,

      <div class="separator"></div>,

      <button
        disabled={this.disabledTitle} onClick={() => this.toggleLink()} class={this.link ? "link active" : "link"}>
        <div></div>
      </button>
    ];
  }

  private renderImageActions() {
    return [
      <button
        onClick={(e: UIEvent) => this.styleImage(e, this.setImageWith, ImageSize.ORIGINAL)}
        class={this.imageSize === ImageSize.ORIGINAL ? "image original active" : "image original"}>
        <div></div>
      </button>,
      <button
        onClick={(e: UIEvent) => this.styleImage(e, this.setImageWith, ImageSize.LARGE)}
        class={this.imageSize === ImageSize.LARGE ? "image large active" : "image large"}>
        <div></div>
      </button>,
      <button
        onClick={(e: UIEvent) => this.styleImage(e, this.setImageWith, ImageSize.MEDIUM)}
        class={this.imageSize === ImageSize.MEDIUM ? "image medium active" : "image medium"}>
        <div></div>
      </button>,
      <button
        onClick={(e: UIEvent) => this.styleImage(e, this.setImageWith, ImageSize.SMALL)}
        class={this.imageSize === ImageSize.SMALL ? "image small active" : "image small"}>
        <div></div>
      </button>,

      <div class="separator"></div>,

      <button
        onClick={(e: UIEvent) => this.styleImage(e, this.setImageAlignment, ImageAlign.STANDARD)}
        class={this.imageAlign === ImageAlign.STANDARD ? "image-align standard active" : "image-align standard"}>
        <div></div>
      </button>,
      <button
        onClick={(e: UIEvent) => this.styleImage(e, this.setImageAlignment, ImageAlign.START)}
        class={this.imageAlign === ImageAlign.START ? "image-align start active" : "image-align start"}>
        <div></div>
      </button>,

      <div class="separator"></div>,

      <button
        onClick={(e: UIEvent) => this.deleteImage(e)} class="image-delete">
        <div></div>
      </button>


    ];
  }
}
