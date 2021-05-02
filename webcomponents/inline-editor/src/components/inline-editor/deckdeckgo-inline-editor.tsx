import {Component, Element, Event, EventEmitter, h, Host, Listen, Method, Prop, State, Watch} from '@stencil/core';

import {debounce, isIOS, isMobile, isRTL, unifyEvent} from '@deckdeckgo/utils';

import '@deckdeckgo/color';
import {DeckdeckgoPalette, DEFAULT_PALETTE} from '@deckdeckgo/color';

import {ContentAlign, ContentList, FontSize, ToolbarActions} from '../../types/enums';

import {AnchorLink, ExecCommandAction, InlineAction} from '../../interfaces/interfaces';

import {DeckdeckgoInlineEditorUtils} from '../../utils/utils';
import {execCommand} from '../../utils/execcommand.utils';
import {clearTheSelection, getSelection} from '../../utils/selection.utils';

/**
 * @slot - related to the customActions propery
 */
@Component({
  tag: 'deckgo-inline-editor',
  styleUrl: 'deckdeckgo-inline-editor.scss',
  shadow: true,
})
export class DeckdeckgoInlineEditor {
  @Element() el: HTMLElement;

  /**
   * In case you would like to define a custom list of colors for the palette of colors. See @deckdeckgo/color for the default list of colors
   */
  @Prop() palette: DeckdeckgoPalette[] = DEFAULT_PALETTE;

  @State()
  private bold: 'bold' | 'initial' | undefined = undefined;

  @State()
  private italic: 'italic' | 'initial' | undefined = undefined;

  @State()
  private underline: 'underline' | 'initial' | undefined = undefined;

  @State()
  private strikethrough: 'strikethrough' | 'initial' | undefined = undefined;

  @State()
  private contentAlign: ContentAlign;

  @State()
  private contentList: ContentList | undefined = undefined;

  @State()
  private contentFontSize: FontSize | undefined = undefined;

  @State()
  private disabledTitle: boolean = false;

  /**
   * The mobile mode is automatically recognize, but just it case you would like to "force" it
   */
  @Prop({mutable: true})
  mobile: boolean = false;

  /**
   * Use a sticky footer toolbar on desktop
   */
  @Prop()
  stickyDesktop: boolean = false;

  /**
   * Use a sticky footer toolbar on mobile. The sticky bar is positioned bottom except on iOS for which it will be positioned top
   */
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

  /**
   * Could be use to attach the inline editor event listeners (mousedown, touchstart and keydown) to a specific element instead of the document
   */
  @Prop()
  attachTo: HTMLElement;

  /**
   * A comma separated list of containers where the inline editor should/could be use. Used in order to allow the component to detect some information like the current style or color
   */
  @Prop()
  containers: string = 'h1,h2,h3,h4,h5,h6,div';

  /**
   * Triggered when an image is manipulated. Note: the event won't provide directly the image but rather its container element
   */
  @Event()
  imgDidChange: EventEmitter<HTMLElement>;

  /**
   * Triggered when a link is created by the user. The event detail is the container
   */
  @Event()
  linkCreated: EventEmitter<HTMLElement>;

  /**
   * Triggered when the style is modified (bold, italic, color, alignment, etc.). The event detail is the container
   */
  @Event()
  styleDidChange: EventEmitter<HTMLElement>;

  /**
   * The type of element to attach the image toolbar
   */
  @Prop()
  imgAnchor: string = 'img';

  /**
   * In case you would like to use a specific property to specify the width on your image
   */
  @Prop()
  imgPropertyWidth: string = 'width';

  /**
   * In case you would like to use a specific property to specify the float on your image
   */
  @Prop()
  imgPropertyCssFloat: string = 'float';

  private iOSTimerScroll: number;

  /**
   * Per default, the component will not consider images as editable. Turn this option to true to activate the edition of images
   */
  @Prop()
  imgEditable: boolean = false;

  /**
   * Actions to manipulate the selection as list enabled?
   */
  @Prop()
  list: boolean = true;

  /**
   * Actions to manipulat
   */
  @Prop()
  align: boolean = true;

  /**
   * Actions to modify the selection font-size enabled?
   */
  @Prop()
  fontSize: boolean = true;

  /**
   * To hide the option to select a background-color
   */
  @Prop()
  backgroundColor: boolean = true;

  /**
   * You might to display and add further actions to the component ? Use this property to provide a comma separated list of actions
   */
  @Prop()
  customActions: string; // Comma separated list of additional action components

  /**
   * Triggered when a custom action is selected. Its detail provide an action name, the Selection and an anchorLink
   */
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

  async disconnectedCallback() {
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
      const selection: Selection | undefined = await getSelection();

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
        this.bold = undefined;
        this.italic = undefined;
        this.underline = undefined;
        this.strikethrough = undefined;
        this.contentList = undefined;
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

        this.contentAlign = await DeckdeckgoInlineEditorUtils.getContentAlignment(node as HTMLElement);

        resolve();
      } else {
        if (this.bold === undefined) {
          this.bold = await DeckdeckgoInlineEditorUtils.getBold(node as HTMLElement);
        }

        if (this.italic === undefined) {
          this.italic = await DeckdeckgoInlineEditorUtils.getItalic(node as HTMLElement);
        }

        if (this.underline === undefined) {
          this.underline = await DeckdeckgoInlineEditorUtils.getUnderline(node as HTMLElement);
        }

        if (this.strikethrough === undefined) {
          this.strikethrough = await DeckdeckgoInlineEditorUtils.getStrikeThrough(node as HTMLElement);
        }

        if (this.contentList === undefined) {
          this.contentList = await DeckdeckgoInlineEditorUtils.getList(node as HTMLElement);
        }

        await this.findStyle(node.parentNode);

        if (this.contentFontSize === undefined) {
          this.contentFontSize = await DeckdeckgoInlineEditorUtils.getFontSize(node as HTMLElement);
        }

        resolve();
      }
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

  /**
   * Reset the inline editor (= hide it) and optionally clear its selection.
   * @param clearSelection
   * @param blurActiveElement
   */
  @Method()
  reset(clearSelection: boolean, blurActiveElement?: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (clearSelection) {
        await clearTheSelection();
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

  private async openColorPicker(action: ToolbarActions.COLOR | ToolbarActions.BACKGROUND_COLOR): Promise<void> {
    this.toolbarActions = action;
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

  private async onExecCommand($event: CustomEvent<ExecCommandAction>) {
    if (!$event || !$event.detail) {
      return;
    }

    await execCommand(this.selection, $event.detail, this.containers);

    const container: HTMLElement = await DeckdeckgoInlineEditorUtils.findContainer(
      this.containers,
      !this.selection ? document.activeElement : this.selection.anchorNode
    );
    this.styleDidChange.emit(container);

    await this.reset(true);
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
          containers={this.containers}
          mobile={this.mobile}
          onLinkModified={($event: CustomEvent<boolean>) => this.reset($event.detail)}></deckgo-ie-link-actions>
      );
    } else if (this.toolbarActions === ToolbarActions.COLOR || this.toolbarActions === ToolbarActions.BACKGROUND_COLOR) {
      return (
        <deckgo-ie-color-actions
          selection={this.selection}
          action={this.toolbarActions === ToolbarActions.BACKGROUND_COLOR ? 'background-color' : 'color'}
          palette={this.palette}
          mobile={this.mobile}
          onExecCommand={($event: CustomEvent<ExecCommandAction>) => this.onExecCommand($event)}></deckgo-ie-color-actions>
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
          onExecCommand={($event: CustomEvent<ExecCommandAction>) => this.onExecCommand($event)}></deckgo-ie-list-actions>
      );
    } else if (this.toolbarActions === ToolbarActions.FONT_SIZE) {
      return (
        <deckgo-ie-font-size-actions
          mobile={this.mobile}
          sticky={sticky}
          fontSize={this.contentFontSize}
          onExecCommand={($event: CustomEvent<ExecCommandAction>) => this.onExecCommand($event)}></deckgo-ie-font-size-actions>
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
        bold={this.bold === 'bold'}
        italic={this.italic === 'italic'}
        underline={this.underline === 'underline'}
        strikethrough={this.strikethrough === 'strikethrough'}
        onExecCommand={($event: CustomEvent<ExecCommandAction>) => this.onExecCommand($event)}></deckgo-ie-style-actions>,

      this.renderSeparator(),

      this.renderFontSizeAction(),

      this.renderSeparator(),

      this.renderColorActions(),

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

  private renderColorActions() {
    const result = [
      <deckgo-ie-action-button mobile={this.mobile} onAction={() => this.openColorPicker(ToolbarActions.COLOR)}>
        <deckgo-ie-action-image cssClass={'pick-color'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,
    ];

    if (this.backgroundColor) {
      result.push(
        <deckgo-ie-action-button mobile={this.mobile} onAction={() => this.openColorPicker(ToolbarActions.BACKGROUND_COLOR)}>
          <deckgo-ie-action-image cssClass={'pick-background'}></deckgo-ie-action-image>
        </deckgo-ie-action-button>
      );
    }

    return result;
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
