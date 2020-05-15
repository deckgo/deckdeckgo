import {Component, Element, Event, EventEmitter, h, Host, Listen, Method, Prop, State, Watch} from '@stencil/core';

import {debounce, isIOS, isMobile, isRTL, unifyEvent} from '@deckdeckgo/utils';

import '@deckdeckgo/color';
import {DeckdeckgoPalette, DEFAULT_PALETTE} from '@deckdeckgo/color';

import {ContentAlign, ContentList, FontSize, ToolbarActions} from '../../types/enums';
import {AnchorLink, InlineAction} from '../../interfaces/interfaces';

import {DeckdeckgoInlineEditorUtils} from '../../utils/utils';

@Component({
  tag: 'deckgo-inline-editor',
  styleUrl: 'deckdeckgo-inline-editor.scss',
  shadow: true,
})
export class DeckdeckgoInlineEditor {
  @Element() el: HTMLElement;

  @Prop() palette: DeckdeckgoPalette[] = DEFAULT_PALETTE;

  @State()
  private bold: boolean = false;

  @State()
  private italic: boolean = false;

  @State()
  private underline: boolean = false;

  @State()
  private contentAlign: ContentAlign;

  @State()
  private contentList: ContentList | undefined = undefined;

  @State()
  private color: string;

  @State()
  private contentFontSize: FontSize | undefined = undefined;

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

  @State()
  private displayToolsActivated: boolean = false;

  private debounceDisplayToolsActivated: Function;

  private selection: Selection = null;

  private anchorLink: AnchorLink = null;
  private anchorEvent: MouseEvent | TouchEvent;

  @State()
  private link: boolean = false;

  @State()
  private toolbarActions: ToolbarActions = ToolbarActions.SELECTION;

  @Event() stickyToolbarActivated: EventEmitter<boolean>;

  @Prop()
  attachTo: HTMLElement;

  @Prop()
  containers: string = 'h1,h2,h3,h4,h5,h6,div';

  @Event() private imgDidChange: EventEmitter<HTMLElement>;

  @Event() private linkCreated: EventEmitter<HTMLElement>;

  @Prop()
  imgAnchor: string = 'img';

  @Prop()
  imgPropertyWidth: string = 'width';

  @Prop()
  imgPropertyCssFloat: string = 'float';

  private iOSTimerScroll: number;

  @Prop()
  imgEditable: boolean = false;

  @Prop()
  list: boolean = true;

  @Prop()
  align: boolean = true;

  @Prop()
  fontSize: boolean = true;

  @Prop()
  customActions: string; // Comma separated list of additional action components

  @Event()
  customAction: EventEmitter<InlineAction>;

  tools!: HTMLDivElement;

  @State()
  private toolsLeft: number;

  @State()
  private toolsTop: number;

  @State()
  private anchorEventLeft: number = 0;

  private rtl: boolean = isRTL();

  constructor() {
    this.resetDisplayToolsActivated();
  }

  private resetDisplayToolsActivated() {
    this.debounceDisplayToolsActivated = debounce(() => {
      this.displayToolsActivated = true;
    });
  }

  async componentWillLoad() {
    await this.attachListener();

    await this.initDefaultContentAlign();
  }

  async componentDidLoad() {
    if (!this.mobile) {
      this.mobile = isMobile();
    }
  }

  async componentDidUnload() {
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
        listenerElement.addEventListener('mousedown', this.startSelection, {passive: true});
        listenerElement.addEventListener('touchstart', this.startSelection, {passive: true});
      }

      resolve();
    });
  }

  private detachListener(listenerElement: HTMLElement | Document): Promise<void> {
    return new Promise<void>((resolve) => {
      if (listenerElement) {
        listenerElement.removeEventListener('mousedown', this.startSelection);
        listenerElement.removeEventListener('touchstart', this.startSelection);
      }

      resolve();
    });
  }

  private startSelection = async ($event: MouseEvent | TouchEvent) => {
    if (this.displayToolsActivated) {
      return;
    }

    if (this.toolbarActions !== ToolbarActions.IMAGE) {
      this.anchorEvent = $event;
    }

    if (this.toolsActivated) {
      await this.resetImageToolbarActions($event);

      return;
    }

    if (this.toolbarActions === ToolbarActions.IMAGE) {
      this.anchorEvent = $event;
    }

    await this.displayImageActions($event);
  };

  private resetImageToolbarActions($event: MouseEvent | TouchEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.toolbarActions !== ToolbarActions.IMAGE) {
        resolve();
        return;
      }

      if ($event && $event.target && $event.target instanceof HTMLElement) {
        const target: HTMLElement = $event.target as HTMLElement;

        if (target && target.nodeName && target.nodeName.toLowerCase() !== 'deckgo-inline-editor') {
          await this.reset(false);
        }
      }

      resolve();
    });
  }

  private displayImageActions($event: MouseEvent | TouchEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.imgEditable) {
        resolve();
        return;
      }

      const isAnchorImg: boolean = await this.isAnchorImage();
      if (!isAnchorImg) {
        resolve();
        return;
      }

      $event.stopImmediatePropagation();

      await this.reset(true);

      setTimeout(
        async () => {
          await this.activateToolbarImage();
          await this.setToolbarAnchorPosition();
        },
        this.mobile ? 300 : 100
      );

      resolve();
    });
  }

  private activateToolbarImage(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.toolbarActions = ToolbarActions.IMAGE;
      this.color = undefined;
      await this.setToolsActivated(true);

      resolve();
    });
  }

  private isAnchorImage(): Promise<boolean> {
    return DeckdeckgoInlineEditorUtils.isAnchorImage(this.anchorEvent, this.imgAnchor);
  }

  @Listen('selectionchange', {target: 'document', passive: true})
  async selectionchange(_$event: UIEvent) {
    if (document && document.activeElement && !this.isContainer(document.activeElement)) {
      if (document.activeElement.nodeName.toLowerCase() !== 'deckgo-inline-editor') {
        await this.reset(false);
      }

      return;
    }

    const anchorImage: boolean = await this.isAnchorImage();
    if (this.toolbarActions === ToolbarActions.IMAGE && anchorImage) {
      await this.reset(false);
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

      if (this.attachTo && !this.attachTo.contains(this.anchorEvent.target as Node)) {
        await this.reset(false);
        resolve();
        return;
      }

      if (!selection || !selection.toString() || selection.toString().trim().length <= 0) {
        await this.reset(false);
        resolve();
        return;
      }

      const activated: boolean = await this.activateToolbar(selection);
      await this.setToolsActivated(activated);

      if (this.toolsActivated) {
        this.selection = selection;

        if (selection.rangeCount > 0) {
          const range: Range = selection.getRangeAt(0);
          this.anchorLink = {
            range: range,
            text: selection.toString(),
            element: document.activeElement,
          };

          await this.setToolbarAnchorPosition();
        }
      }

      resolve();
    });
  }

  private setToolbarAnchorPosition(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.isSticky()) {
        await this.handlePositionIOS();

        resolve();
        return;
      }

      if (this.tools) {
        let top: number = unifyEvent(this.anchorEvent).clientY;
        let left: number = unifyEvent(this.anchorEvent).clientX - 40;

        if (this.mobile) {
          top = top + 40;
        } else {
          top = top + 24;
        }

        const innerWidth: number = isIOS() ? screen.width : window.innerWidth;

        if (innerWidth > 0 && left > innerWidth - this.tools.offsetWidth) {
          left = innerWidth - this.tools.offsetWidth;
        }

        if (left < 0) {
          left = 0;
        }

        // To set the position of the tools
        this.toolsTop = top;
        this.toolsLeft = left;

        // To set the position of the triangle
        this.anchorEventLeft = left > 0 ? unifyEvent(this.anchorEvent).clientX - 20 - left : unifyEvent(this.anchorEvent).clientX;
      }

      resolve();
    });
  }

  private handlePositionIOS(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!isIOS() || !this.anchorEvent) {
        resolve();
        return;
      }

      await this.setStickyPositionIOS();

      if (window) {
        window.addEventListener(
          'scroll',
          async () => {
            await this.setStickyPositionIOS();
          },
          {passive: true}
        );
        window.addEventListener(
          'resize',
          async () => {
            await this.reset(true, true);
          },
          {passive: true}
        );
      }
    });
  }

  private setStickyPositionIOS(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.stickyMobile || !isIOS() || !window) {
        resolve();
        return;
      }

      if (this.iOSTimerScroll > 0) {
        clearTimeout(this.iOSTimerScroll);
      }

      this.iOSTimerScroll = setTimeout(() => {
        this.el.style.setProperty('--deckgo-inline-editor-sticky-scroll', `${window.scrollY}px`);
      }, 50);

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

      if (this.isContainer(content) || content.parentElement) {
        this.bold = false;
        this.italic = false;
        this.underline = false;
        this.contentList = undefined;
        this.color = null;
        this.contentFontSize = undefined;

        await this.initDefaultContentAlign();

        await this.findStyle(this.isContainer(content) ? content : content.parentElement);
      }

      resolve();
    });
  }

  private async initDefaultContentAlign() {
    this.contentAlign = this.rtl ? ContentAlign.RIGHT : ContentAlign.LEFT;
  }

  private isContainer(element: Node): boolean {
    return DeckdeckgoInlineEditorUtils.isContainer(this.containers, element);
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
        this.contentAlign = await DeckdeckgoInlineEditorUtils.getContentAlignment(node as HTMLElement);

        resolve();
      } else {
        this.bold = await DeckdeckgoInlineEditorUtils.isBold(node as HTMLElement);
        this.italic = await DeckdeckgoInlineEditorUtils.isItalic(node as HTMLElement);
        this.underline = await DeckdeckgoInlineEditorUtils.isUnderline(node as HTMLElement);

        if (this.contentList === undefined) {
          this.contentList = await DeckdeckgoInlineEditorUtils.isList(node as HTMLElement);
        }

        await this.findColor(node);

        await this.findStyle(node.parentNode);

        if (this.contentFontSize === undefined) {
          this.contentFontSize = await DeckdeckgoInlineEditorUtils.getFontSize(node as HTMLElement);
        }

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

  @Method()
  reset(clearSelection: boolean, blurActiveElement?: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (clearSelection) {
        await this.clearTheSelection();
      }

      await this.setToolsActivated(false);

      this.resetDisplayToolsActivated();

      this.selection = null;

      this.toolbarActions = ToolbarActions.SELECTION;
      this.anchorLink = null;
      this.link = false;

      if (window) {
        window.removeEventListener('scroll', async () => {
          await this.setStickyPositionIOS();
        });
        window.removeEventListener('resize', async () => {
          await this.reset(true, true);
        });
      }

      if (blurActiveElement && document && document.activeElement && document.activeElement instanceof HTMLElement) {
        document.activeElement.blur();
      }

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

  private isSticky(): boolean {
    const mobile: boolean = isMobile();

    return (this.stickyDesktop && !mobile) || (this.stickyMobile && mobile);
  }

  private setToolsActivated(activated: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.toolsActivated = activated;

      if (activated) {
        this.debounceDisplayToolsActivated();
      } else {
        this.displayToolsActivated = false;
      }

      if (this.isSticky()) {
        this.stickyToolbarActivated.emit(this.toolsActivated);
      }

      resolve();
    });
  }

  private async openColorPicker(): Promise<void> {
    this.toolbarActions = ToolbarActions.COLOR;
  }

  private async openAlignmentActions(): Promise<void> {
    this.toolbarActions = ToolbarActions.ALIGNMENT;
  }

  private async openFontSizeActions(): Promise<void> {
    this.toolbarActions = ToolbarActions.FONT_SIZE;
  }

  private async openListActions(): Promise<void> {
    this.toolbarActions = ToolbarActions.LIST;
  }

  private async onCustomAction($event: UIEvent, action: string): Promise<void> {
    $event.stopPropagation();

    this.customAction.emit({
      action: action,
      selection: this.selection,
      anchorLink: this.anchorLink,
    });
  }

  render() {
    let classNames: string = this.displayToolsActivated
      ? this.mobile
        ? 'deckgo-tools deckgo-tools-activated deckgo-tools-mobile'
        : 'deckgo-tools deckgo-tools-activated'
      : this.mobile
      ? 'deckgo-tools deckgo-tools-mobile'
      : 'deckgo-tools';

    if (this.isSticky()) {
      classNames += ' deckgo-tools-sticky';
    }

    const hostClass = isIOS() ? 'deckgo-tools-ios' : undefined;

    return (
      <Host class={hostClass}>
        <div class={classNames} ref={(el) => (this.tools = el as HTMLDivElement)} style={{left: `${this.toolsLeft}px`, top: `${this.toolsTop}px`}}>
          <deckgo-ie-triangle style={{'--deckgo-ie-triangle-start': `${this.anchorEventLeft}px`}}></deckgo-ie-triangle>
          {this.renderActions()}
        </div>
      </Host>
    );
  }

  private renderActions() {
    const sticky: boolean = this.isSticky();

    if (this.toolbarActions === ToolbarActions.LINK) {
      return (
        <deckgo-ie-link-actions
          toolbarActions={this.toolbarActions}
          anchorLink={this.anchorLink}
          selection={this.selection}
          linkCreated={this.linkCreated}
          mobile={this.mobile}
          onLinkModified={($event: CustomEvent<boolean>) => this.reset($event.detail)}></deckgo-ie-link-actions>
      );
    } else if (this.toolbarActions === ToolbarActions.COLOR) {
      return (
        <deckgo-ie-color-actions
          selection={this.selection}
          color={this.color}
          palette={this.palette}
          mobile={this.mobile}
          onColorModified={() => this.reset(true)}></deckgo-ie-color-actions>
      );
    } else if (this.toolbarActions === ToolbarActions.IMAGE) {
      return (
        <deckgo-ie-image-actions
          anchorEvent={this.anchorEvent}
          imgPropertyWidth={this.imgPropertyWidth}
          imgPropertyCssFloat={this.imgPropertyCssFloat}
          imgDidChange={this.imgDidChange}
          containers={this.containers}
          imgAnchor={this.imgAnchor}
          mobile={this.mobile}
          onImgModified={() => this.reset(true)}></deckgo-ie-image-actions>
      );
    } else if (this.toolbarActions === ToolbarActions.ALIGNMENT) {
      return (
        <deckgo-ie-align-actions
          anchorEvent={this.anchorEvent}
          containers={this.containers}
          mobile={this.mobile}
          sticky={sticky}
          contentAlign={this.contentAlign}
          onAlignModified={() => this.reset(true)}></deckgo-ie-align-actions>
      );
    } else if (this.toolbarActions === ToolbarActions.LIST) {
      return (
        <deckgo-ie-list-actions
          selection={this.selection}
          disabledTitle={this.disabledTitle}
          mobile={this.mobile}
          sticky={sticky}
          contentList={this.contentList}
          onListModified={() => this.reset(true)}></deckgo-ie-list-actions>
      );
    } else if (this.toolbarActions === ToolbarActions.FONT_SIZE) {
      return (
        <deckgo-ie-font-size-actions
          mobile={this.mobile}
          sticky={sticky}
          fontSize={this.contentFontSize}
          onFontSizeModified={() => this.reset(true)}></deckgo-ie-font-size-actions>
      );
    } else {
      return this.renderSelectionActions();
    }
  }

  private renderSelectionActions() {
    return [
      <deckgo-ie-style-actions
        mobile={this.mobile}
        disabledTitle={this.disabledTitle}
        selection={this.selection}
        bold={this.bold}
        italic={this.italic}
        underline={this.underline}
        onInitStyle={() => this.initStyle(this.selection)}></deckgo-ie-style-actions>,

      this.renderFontSizeAction(),

      <deckgo-ie-action-button mobile={this.mobile} onAction={() => this.openColorPicker()}>
        <deckgo-ie-action-image cssClass={'pick-color'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,

      this.renderSeparator(),

      this.renderAlignAction(),

      this.renderListAction(),

      this.renderLinkSeparator(),

      <deckgo-ie-action-button mobile={this.mobile} onAction={() => this.toggleLink()} cssClass={this.link ? 'active' : undefined}>
        <deckgo-ie-action-image cssClass={'link'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,

      this.renderCustomActions(),
    ];
  }

  private renderSeparator() {
    return <deckgo-ie-separator mobile={this.mobile}></deckgo-ie-separator>;
  }

  private renderLinkSeparator() {
    if (!this.list && !this.align) {
      return undefined;
    }

    return this.renderSeparator();
  }

  private renderCustomActions() {
    return this.customActions ? this.customActions.split(',').map((customAction: string) => this.renderCustomAction(customAction)) : undefined;
  }

  private renderCustomAction(customAction: string) {
    return [
      this.renderSeparator(),
      <deckgo-ie-action-button mobile={this.mobile} onClick={($event: UIEvent) => this.onCustomAction($event, customAction)}>
        <slot name={customAction}></slot>
      </deckgo-ie-action-button>,
    ];
  }

  private renderListAction() {
    if (!this.list) {
      return undefined;
    }

    return (
      <deckgo-ie-action-button mobile={this.mobile} onAction={() => this.openListActions()}>
        <deckgo-ie-action-image cssClass={this.contentList === ContentList.UNORDERED ? 'unordered-list' : 'ordered-list'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>
    );
  }

  private renderAlignAction() {
    if (!this.align) {
      return undefined;
    }

    return (
      <deckgo-ie-action-button mobile={this.mobile} onAction={() => this.openAlignmentActions()}>
        <deckgo-ie-action-image
          cssClass={
            this.contentAlign === ContentAlign.LEFT ? 'left-align' : this.contentAlign === ContentAlign.CENTER ? 'center-align' : 'right-align'
          }></deckgo-ie-action-image>
      </deckgo-ie-action-button>
    );
  }

  private renderFontSizeAction() {
    if (!this.fontSize) {
      return undefined;
    }

    return (
      <deckgo-ie-action-button mobile={this.mobile} onAction={() => this.openFontSizeActions()}>
        <span>
          A<small>A</small>
        </span>
      </deckgo-ie-action-button>
    );
  }
}
