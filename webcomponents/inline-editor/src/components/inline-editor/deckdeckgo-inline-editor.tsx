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

  @State()
  private toolsActivated: boolean = false;

  private selection: Selection = null;
  private anchorEvent: MouseEvent | TouchEvent;

  componentDidLoad() {
    if (!this.mobile) {
      this.mobile = this.isMobile();
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

          tools.style.top = '' + (top) + 'px';
          tools.style.left = '' + (left) + 'px';
        }
      }

      resolve();
    });
  }

  isMobile(): boolean {
    if (!window || !navigator) {
      return false;
    }

    const a: string = navigator.userAgent || navigator.vendor || (window as any).opera;

    return (/(android|bb\d+|meego).+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|mobile.+firefox|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\.(browser|link)|vodafone|wap|windows ce|xda|xiino/i.test(a) || /1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1 u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp( i|ip)|hs\-c|ht(c(\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac( |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\/)|klon|kpt |kwc\-|kyo(c|k)|le(no|xi)|lg( g|\/(k|l|u)|50|54|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\-|your|zeto|zte\-/i.test(a.substr(0, 4)));
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

      this.type = ContentType[container.nodeName.toUpperCase()];

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

      if (ContentType[content.nodeName.toUpperCase()]) {
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
      if (node.nodeName.toUpperCase() === 'HTML' || node.nodeName.toUpperCase() === 'BODY') {
        resolve();
        return;
      }

      if (ContentType[node.nodeName.toUpperCase()]) {
        const children: HTMLCollection = (node as HTMLElement).children;
        this.styledElements = children && children.length > 0;
        resolve();
      } else {
        if (node.nodeName.toUpperCase() === 'B') {
          this.bold = true;
        } else if (node.nodeName.toUpperCase() === 'I') {
          this.italic = true;
        } else if (node.nodeName.toUpperCase() === 'U') {
          this.underline = true;
        }

        const boldChildren: HTMLCollection = (node as HTMLElement).getElementsByTagName('b');
        const italicChildren: HTMLCollection = (node as HTMLElement).getElementsByTagName('i');
        const underlineChildren: HTMLCollection = (node as HTMLElement).getElementsByTagName('u');

        this.bold = this.bold || (boldChildren && boldChildren.length > 0);
        this.italic = this.italic || (italicChildren && italicChildren.length > 0);
        this.underline = this.underline || (underlineChildren && underlineChildren.length > 0);

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

  // TODO: Detect iPad
  // TODO: link

  render() {
    const classNames: string = this.toolsActivated ? (this.mobile ? 'deckgo-tools deckgo-tools-activated deckgo-tools-mobile' : 'deckgo-tools deckgo-tools-activated') : (this.mobile ? 'deckgo-tools deckgo-tools-mobile' : 'deckgo-tools');

    return (<div class={classNames}>
      <button onClick={(e: UIEvent) => this.styleBold(e)} disabled={this.type !== undefined && this.type !== ContentType.P} class={this.bold ? "bold active" : "bold"}>B</button>
      <button onClick={(e: UIEvent) => this.styleItalic(e)} disabled={this.type !== undefined && this.type !== ContentType.P} class={this.italic ? "italic active" : "italic"}>I</button>
      <button onClick={(e: UIEvent) => this.styleUnderline(e)} disabled={this.type !== undefined && this.type !== ContentType.P} class={this.underline ? "underline active" : "underline"}>U</button>

      <div class="separator"></div>

      <button onClick={(e: UIEvent) => this.toggle(e, ContentType.H1)} disabled={this.bold || this.italic || this.underline || this.styledElements} class={this.type === ContentType.H1 ? "h1 active" : "h1"}>T</button>
      <button onClick={(e: UIEvent) => this.toggle(e, ContentType.H2)} disabled={this.bold || this.italic || this.underline || this.styledElements} class={this.type === ContentType.H2 ? "h2 active" : "h2"}>T</button>
      <button onClick={(e: UIEvent) => this.toggle(e, ContentType.H3)} disabled={this.bold || this.italic || this.underline || this.styledElements} class={this.type === ContentType.H3 ? "h3 active" : "h3"}>T</button>
    </div>);
  }

}
