import {Component, Element, Event, EventEmitter, h, Listen, Method, Prop, State} from '@stencil/core';
import {modalController, OverlayEventDetail, popoverController} from '@ionic/core';

import {debounce, isFullscreen, isIOS, isMobile} from '@deckdeckgo/utils';

import store from '../../../../../stores/busy.store';

import {ImageHelper} from '../../../../../helpers/editor/image.helper';
import {ShapeHelper} from '../../../../../helpers/editor/shape.helper';

import {SlideSplitType} from '../../../../../models/data/slide';

import {ToggleSlotUtils} from '../../../../../utils/editor/toggle-slot.utils';
import {RevealSlotUtils} from '../../../../../utils/editor/reveal-slot.utils';
import {SlotType} from '../../../../../utils/editor/slot-type';
import {SlotUtils} from '../../../../../utils/editor/slot.utils';

import {EditAction} from '../../../../../utils/editor/edit-action';
import {MoreAction} from '../../../../../utils/editor/more-action';
import {DemoAction} from '../../../../../utils/editor/demo-action';
import {PlaygroundAction} from '../../../../../utils/editor/playground-action';

@Component({
  tag: 'app-actions-element',
  styleUrl: 'app-actions-element.scss',
  shadow: false,
})
export class AppActionsElement {
  @Element() el: HTMLElement;

  @Prop()
  slideCopy: EventEmitter;

  @Prop()
  elementFocus: EventEmitter;

  private selectedElement: HTMLElement;

  @State()
  private slide: boolean = false;

  @State()
  private slideNodeName: string | undefined;

  @State()
  private slideDemo: boolean = false;

  @State()
  private code: boolean = false;

  @State()
  private math: boolean = false;

  private wordCloud: boolean = false;

  private markdown: boolean = false;

  @State()
  private image: boolean = false;

  @State()
  private shape: 'shape' | 'text' | undefined = undefined;

  @Event() private blockSlide: EventEmitter<boolean>;

  @Event() private slideDelete: EventEmitter<HTMLElement>;

  @Event() private slideDidChange: EventEmitter<HTMLElement>;
  @Event() private codeDidChange: EventEmitter<HTMLElement>;
  @Event() private mathDidChange: EventEmitter<HTMLElement>;
  @Event() private imgDidChange: EventEmitter<HTMLElement>;
  @Event() private notesDidChange: EventEmitter<HTMLElement>;

  private elementResizeObserver: ResizeObserverConstructor;

  private readonly debounceResizeSlideContent: () => void;

  @Event() signIn: EventEmitter<void>;

  private imageHelper: ImageHelper;
  private shapeHelper: ShapeHelper;

  @Event() private resetted: EventEmitter<void>;

  constructor() {
    this.debounceResizeSlideContent = debounce(async () => {
      await this.resizeSlideContent();
    }, 250);
  }

  async componentWillLoad() {
    this.imageHelper = new ImageHelper(this.slideDidChange, this.blockSlide, this.signIn);
    this.shapeHelper = new ShapeHelper(this.slideDidChange, this.signIn);
  }

  async componentDidLoad() {
    this.initWindowResize();
  }

  async disconnectedCallback() {
    this.removeWindowResize();
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', () => this.debounceResizeSlideContent(), {passive: true});
    }
  }

  private removeWindowResize() {
    if (window) {
      window.removeEventListener('resize', () => this.debounceResizeSlideContent(), true);
    }
  }

  @Listen('mouseInactivity', {target: 'document'})
  async inactivity(_$event: CustomEvent) {
    if (!isFullscreen()) {
      return;
    }

    await this.blurSelectedElement();
  }

  @Listen('optionsDidChange', {target: 'document'})
  async onOptionsDidChange() {
    await this.emitChange();
  }

  @Listen('toggleReveal', {target: 'document'})
  async onToggleReveal($event: CustomEvent<boolean>) {
    if (!this.selectedElement || !this.selectedElement.parentElement || !$event) {
      return;
    }

    const element: HTMLElement = await RevealSlotUtils.toggleReveal(this.selectedElement, $event.detail);

    await this.replaceSlot(element);
  }

  @Listen('toggleList', {target: 'document'})
  async onToggleList($event: CustomEvent<SlotType.OL | SlotType.UL>) {
    if (!$event) {
      return;
    }

    await this.toggleList($event.detail);
  }

  @Method()
  touch(element: HTMLElement, autoOpen: boolean = true): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.unSelect();
      await this.select(element, autoOpen);

      if (element) {
        element.focus();
      }

      resolve();
    });
  }

  @Method()
  async blurSelectedElement() {
    this.selectedElement?.blur();
  }

  private select(element: HTMLElement, autoOpen: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const selected: HTMLElement = await this.findSelectedElement(element);

      if (!selected) {
        this.blockSlide.emit(false);
        resolve();
        return;
      }

      await this.initSelectedElement(selected);

      // In case of slot deckgo-lazy-img, if user doesn't have yet defined a src for the image, we display the image picker/popover first instead of the toolbar
      if (autoOpen && this.isImgNotDefined(selected)) {
        await this.openImage();
      }

      this.blockSlide.emit(!this.slide);

      resolve();
    });
  }

  private isImgNotDefined(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.IMG && !element.hasAttribute('img-src');
  }

  @Method()
  async unSelect() {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.removeEventListener('paste', this.cleanOnPaste, true);
    await this.detachMoveToolbarOnElement();

    await this.reset();
  }

  private findSelectedElement(element: HTMLElement): Promise<HTMLElement> {
    return new Promise<HTMLElement>(async (resolve) => {
      if (!element || !element.parentElement) {
        resolve(element);
        return;
      }

      if (this.isElementSlide(element)) {
        resolve(element);
        return;
      }

      if (
        element.hasAttribute('slot') &&
        element.getAttribute('slot') !== 'code' &&
        element.getAttribute('slot') !== 'math' &&
        element.getAttribute('slot') !== 'words' &&
        element.getAttribute('slot') !== 'markdown'
      ) {
        resolve(element);
        return;
      }

      const result: HTMLElement = await this.findSelectedElement(element.parentElement);

      resolve(result);
    });
  }

  private isElementSlide(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase().indexOf('deckgo-slide') > -1;
  }

  private isElementCode(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.CODE;
  }

  private isElementMath(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.MATH;
  }

  private isElementWordcloud(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.WORD_CLOUD;
  }

  private isElementMarkdown(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.MARKDOWN;
  }

  private isElementShape(element: HTMLElement): 'shape' | 'text' | undefined {
    if (!element || !element.nodeName || element.nodeName.toLowerCase() !== SlotType.DRAG_RESIZE_ROTATE) {
      return undefined;
    }

    return element.hasAttribute('text') ? 'text' : 'shape';
  }

  private isElementImage(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.IMG;
  }

  private cleanOnPaste = async ($event) => {
    return new Promise<void>(async (resolve) => {
      if (!$event || !document) {
        resolve();
        return;
      }

      const parseText: string = $event.clipboardData.getData('text/plain');

      if (!parseText || parseText.length <= 0) {
        resolve();
        return;
      }

      // In the future, Safari isn't yet ready / without preventDefault
      // // @ts-ignore
      // navigator.permissions.query({name: 'clipboard-write'}).then(async result => {
      //     if (result.state === 'granted' || result.state === 'prompt') {
      //         // @ts-ignore
      //         await navigator.clipboard.writeText(parseText);
      //     }
      // });

      $event.preventDefault();

      const parseTexts: string[] = parseText.replace(/(?:\r\n|\r|\n)/g, '<br/>').split('<br/>');

      if (!parseTexts || parseTexts.length <= 0) {
        resolve();
        return;
      }

      const isCodeElement: boolean = $event.target && $event.target.nodeName && $event.target.nodeName.toLowerCase() === 'code';

      const selected: Selection = await this.getSelection();
      if (selected && selected.rangeCount) {
        const range = selected.getRangeAt(0);
        range.deleteContents();

        parseTexts.forEach((text: string, index: number) => {
          const newTextNode: Text = document.createTextNode(text);
          range.collapse(false);
          range.insertNode(newTextNode);

          if (index < parseTexts.length - 1) {
            range.collapse(false);

            if (isCodeElement) {
              const text: Text = document.createTextNode('\n');
              range.insertNode(text);
            } else {
              const br: HTMLBRElement = document.createElement('br');
              range.insertNode(br);
            }
          }
        });

        selected.empty();

        await this.emitChange();
      }

      resolve();
    });
  };

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

  @Method()
  async reset() {
    await this.initSelectedElement(null);

    this.resetted.emit();
  }

  private async confirmDeleteElement($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-element-delete',
      event: $event,
      mode: isMobile() && !isIOS() ? 'md' : 'ios',
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        await this.deleteElement();
      }
    });

    await popover.present();
  }

  private deleteElement(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (store.state.deckBusy && this.slide) {
        resolve();
        return;
      }

      store.state.deckBusy = true;

      if (this.selectedElement.nodeName && this.selectedElement.nodeName.toLowerCase().indexOf('deckgo-slide') > -1) {
        this.slideDelete.emit(this.selectedElement);
      } else {
        const parent: HTMLElement = this.selectedElement.parentElement;
        this.selectedElement.parentElement.removeChild(this.selectedElement);
        this.slideDidChange.emit(parent);

        await this.resizeSlideContent(parent);
      }

      await this.reset();

      resolve();
    });
  }

  private async clone() {
    if (this.shape !== undefined) {
      await this.cloneShape();
    } else {
      await this.cloneSlide();
    }
  }

  private cloneShape(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (store.state.deckBusy || this.shape === undefined) {
        resolve();
        return;
      }

      await this.shapeHelper.cloneShape(this.selectedElement);

      resolve();
    });
  }

  private cloneSlide(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (store.state.deckBusy || !this.slide) {
        resolve();
        return;
      }

      store.state.deckBusy = true;

      this.slideCopy.emit(this.selectedElement);

      await this.reset();

      resolve();
    });
  }

  private async openTransform() {
    if (this.slide) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-transform',
      componentProps: {
        selectedElement: this.selectedElement,
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data && detail.data.type) {
        await this.transformSlotType(detail.data.type);
      }
    });

    await popover.present();
  }

  private async openEditSlide() {
    if (
      !this.slide ||
      (this.slideNodeName !== 'deckgo-slide-qrcode' && this.slideNodeName !== 'deckgo-slide-chart' && this.slideNodeName !== 'deckgo-slide-author')
    ) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-edit-slide',
      componentProps: {
        selectedElement: this.selectedElement,
        qrCode: this.slideNodeName === 'deckgo-slide-qrcode',
        chart: this.slideNodeName === 'deckgo-slide-chart',
        author: this.slideNodeName === 'deckgo-slide-author',
        slideDidChange: this.slideDidChange,
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        if (detail.data.action === EditAction.DELETE_LOGO) {
          await this.deleteLogo();
        } else if (detail.data.action === EditAction.OPEN_CUSTOM_LOGO) {
          await this.imageHelper.openCustomModalRestricted(this.selectedElement, this.slide, false, 'app-custom-images', detail.data.action);
        } else if (detail.data.action === EditAction.OPEN_DATA) {
          await this.imageHelper.openCustomModalRestricted(this.selectedElement, this.slide, false, 'app-custom-data', detail.data.action);
        }
      }
    });

    await popover.present();
  }

  private async openShape(component: 'app-shape' | 'app-image-element') {
    if (!this.slide || this.slideNodeName !== 'deckgo-slide-aspect-ratio') {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: component,
      componentProps: {
        selectedElement: this.selectedElement,
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        await this.shapeHelper.appendShape(
          this.selectedElement,
          component === 'app-image-element'
            ? {
                img: detail.data,
              }
            : detail.data
        );
      }
    });

    await popover.present();
  }

  private async appendText() {
    await this.shapeHelper.appendText(this.selectedElement);
  }

  private async getImagePopover(): Promise<HTMLIonPopoverElement> {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-image-element',
      componentProps: {
        selectedElement: this.selectedElement,
        slide: this.slide,
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    return popover;
  }

  private async openEditPollSlide() {
    if (!this.slide || this.slideNodeName !== 'deckgo-slide-poll') {
      return;
    }

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-poll-options',
      componentProps: {
        selectedElement: this.selectedElement,
        slideDidChange: this.slideDidChange,
      },
    });

    modal.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async openCode() {
    if (!this.code) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-code',
      componentProps: {
        selectedElement: this.selectedElement,
        codeDidChange: this.codeDidChange,
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    await popover.present();
  }
  private async openMath() {
    if (!this.math) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-math',
      componentProps: {
        selectedElement: this.selectedElement,
        mathDidChange: this.mathDidChange,
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    await popover.present();
  }

  private async openEditModalSlide(
    component: 'app-youtube' | 'app-demo' | 'app-playground',
    onDismiss: (result: string | DemoAction | PlaygroundAction) => Promise<void>
  ) {
    const modal: HTMLIonModalElement = await modalController.create({
      component,
      componentProps: {
        selectedElement: this.selectedElement,
      },
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data && this.selectedElement) {
        await onDismiss(detail.data);
      }

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async openNotes() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-notes',
      componentProps: {
        selectedElement: this.selectedElement,
      },
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data && this.selectedElement) {
        this.notesDidChange.emit(this.selectedElement);
      }

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private transformSlotType(type: SlotType): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || !this.selectedElement.parentElement) {
        resolve();
        return;
      }

      const element: HTMLElement = await ToggleSlotUtils.toggleSlotType(this.selectedElement, type);

      await this.replaceSlot(element);

      resolve();
    });
  }

  private replaceSlot(element: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || !this.selectedElement.parentElement || !element) {
        resolve();
        return;
      }

      this.selectedElement.parentElement.replaceChild(element, this.selectedElement);

      await this.initSelectedElement(element);

      await this.emitChange();

      await this.resizeSlideContent();

      await this.reset();

      resolve();
    });
  }

  private emitChange(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.selectedElement.parentElement) {
        resolve();
        return;
      }

      // If not slide, then parent is the container slide
      this.slideDidChange.emit(this.slide ? this.selectedElement : this.selectedElement.parentElement);

      resolve();
    });
  }

  private initSelectedElement(element: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      // If needed, remove highlight on previous element
      if (!element && this.selectedElement) {
        await this.highlightElement(false);
      }

      this.selectedElement = element;
      this.slide = this.isElementSlide(element);

      this.slideNodeName = this.slide ? element.nodeName.toLowerCase() : undefined;
      this.slideDemo = this.slide && this.slideNodeName === 'deckgo-slide-split' && element.getAttribute('type') === SlideSplitType.DEMO;

      this.math = this.isElementMath(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element);
      this.wordCloud = this.isElementWordcloud(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element);
      this.markdown = this.isElementMarkdown(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element);
      this.code = this.isElementCode(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element);
      this.image = this.isElementImage(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element);
      this.shape = this.isElementShape(element);

      if (element) {
        element.addEventListener('paste', this.cleanOnPaste, false);

        await this.attachMoveToolbarOnElement();

        await this.highlightElement(true);

        this.elementFocus.emit(element);
      }

      resolve();
    });
  }

  private highlightElement(highlight: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      // No highlight on deck
      if (!this.selectedElement || this.slide) {
        resolve();
        return;
      }

      if (highlight) {
        this.selectedElement.setAttribute('highlighted', '');
      } else {
        this.selectedElement.removeAttribute('highlighted');
      }

      resolve();
    });
  }

  private attachMoveToolbarOnElement(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (window && 'ResizeObserver' in window) {
        await this.detachMoveToolbarOnElement();

        this.elementResizeObserver = new ResizeObserver(async (entries) => {
          if (
            entries &&
            entries.length > 0 &&
            entries[0].target &&
            entries[0].target.nodeName &&
            entries[0].target.nodeName.toLowerCase().indexOf('deckgo-slide') === -1
          ) {
            await this.resizeSlideContent();
          }
        });
        this.elementResizeObserver.observe(this.selectedElement);
      } else {
        // Fallback, better  than nothing. It won't place the toolbar if the size on enter or delete  but at least if the content change like if list is toggled
        this.selectedElement.addEventListener('focusout', () => this.debounceResizeSlideContent(), {passive: true});
      }

      resolve();
    });
  }

  private detachMoveToolbarOnElement(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (window && 'ResizeObserver' in window) {
        if (this.elementResizeObserver && this.selectedElement) {
          this.elementResizeObserver.unobserve(this.selectedElement);
          this.elementResizeObserver.disconnect();
        }
      } else {
        this.selectedElement.removeEventListener('focusout', () => this.debounceResizeSlideContent(), true);
      }

      resolve();
    });
  }

  private async openStyle() {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-element-style',
      componentProps: {
        slide: this.slide,
        selectedElement: this.selectedElement,
        imgDidChange: this.imgDidChange,
        imageHelper: this.imageHelper,
        code: this.code,
        math: this.math,
        shape: this.shape,
        image: this.image,
        wordCloud: this.wordCloud,
        markdown: this.markdown,
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: `popover-menu ${this.slideNodeName === 'deckgo-slide-poll' ? 'popover-menu-wide' : ''}`,
    });

    await popover.present();
  }

  private async openImage() {
    const popover: HTMLIonPopoverElement = await this.getImagePopover();

    popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        await this.imageHelper.imageAction(this.selectedElement, this.slide, false, detail.data);
      }
    });

    await popover.present();
  }

  private async deleteLogo() {
    await this.imageHelper.deleteSlideAttributeImgSrc(this.selectedElement);
  }

  private updateYoutube = (youtubeUrl: string): Promise<void> => {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || this.slideNodeName !== 'deckgo-slide-youtube') {
        resolve();
        return;
      }

      if (!youtubeUrl || youtubeUrl === undefined || youtubeUrl === '') {
        resolve();
        return;
      }

      this.selectedElement.setAttribute('src', youtubeUrl);
      this.slideDidChange.emit(this.selectedElement);

      resolve();
    });
  };

  private updatePlayground = async (playground: PlaygroundAction) => {
    if (!this.selectedElement || this.slideNodeName !== 'deckgo-slide-playground') {
      return;
    }

    if (!playground) {
      return;
    }

    if (!playground.src || playground.src === undefined || playground.src === '') {
      return;
    }

    this.selectedElement.setAttribute('src', playground.src);

    if (playground.theme) {
      this.selectedElement.setAttribute('theme', playground.theme);
    } else {
      this.selectedElement.removeAttribute('theme');
    }

    this.slideDidChange.emit(this.selectedElement);
  };

  private updateSlideDemo = async (demoAttr: DemoAction): Promise<void> => {
    if (!this.selectedElement || !this.slideDemo) {
      return;
    }

    if (!demoAttr || !demoAttr.src || demoAttr.src === undefined || demoAttr.src === '') {
      return;
    }

    const demo: HTMLElement = this.selectedElement.querySelector('deckgo-demo');

    if (!demo) {
      return;
    }

    demo.setAttribute('src', demoAttr.src);
    demo.setAttribute('mode', demoAttr.mode);

    this.slideDidChange.emit(this.selectedElement);
  };

  private toggleList(destinationListType: SlotType.OL | SlotType.UL): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (SlotUtils.isNodeRevealList(this.selectedElement)) {
        await this.updateRevealListAttribute(destinationListType);
      } else {
        await this.transformSlotType(destinationListType);
      }

      resolve();
    });
  }

  private updateRevealListAttribute(type: SlotType): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      this.selectedElement.setAttribute('list-tag', type);

      await this.emitChange();

      await this.reset();

      resolve();
    });
  }

  private resizeSlideContent(slideElement?: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      const element: HTMLElement = slideElement ? slideElement : this.slide ? this.selectedElement : this.selectedElement.parentElement;

      if (!element) {
        resolve();
        return;
      }

      if (typeof (element as any).resizeContent === 'function') {
        await (element as any).resizeContent();
      }

      resolve();
    });
  }

  private isSlideEditable() {
    return (
      this.slideNodeName === 'deckgo-slide-qrcode' ||
      this.slideNodeName === 'deckgo-slide-chart' ||
      this.slideNodeName === 'deckgo-slide-poll' ||
      this.slideNodeName === 'deckgo-slide-youtube' ||
      this.slideNodeName === 'deckgo-slide-playground' ||
      this.slideNodeName === 'deckgo-slide-author' ||
      this.slideDemo
    );
  }

  private async openMoreActions($event: UIEvent) {
    if (!$event) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-more-element-actions',
      componentProps: {
        notes: this.slide,
        copy: this.slide || this.shape !== undefined,
        images: this.slideNodeName === 'deckgo-slide-aspect-ratio',
      },
      event: $event,
      mode: 'ios',
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        if (detail.data.action === MoreAction.NOTES) {
          await this.openNotes();
        } else if (detail.data.action === MoreAction.COPY) {
          await this.clone();
        } else if (detail.data.action === MoreAction.DELETE) {
          await this.confirmDeleteElement($event);
        } else if (detail.data.action === MoreAction.IMAGES) {
          await this.openShape('app-image-element');
        }
      }
    });

    await popover.present();
  }

  render() {
    return (
      <ion-toolbar>
        <ion-buttons slot="start">
          {this.renderStyle()}
          {this.renderEdit()}
          {this.renderAspectRatio()}
          {this.renderImages()}
          {this.renderCodeOptions()}
          {this.renderMathOptions()}
          {this.renderTransform()}
        </ion-buttons>

        <ion-buttons slot="end">
          {this.renderNotes()}
          {this.renderCopy()}
          {this.renderDelete()}
          {this.renderMore()}
        </ion-buttons>
      </ion-toolbar>
    );
  }

  private renderDelete() {
    return (
      <button
        onClick={($event: UIEvent) => this.confirmDeleteElement($event)}
        aria-label="Delete"
        disabled={store.state.deckBusy && this.slide}
        class="wider-devices ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/trash-bin.svg"></ion-icon>
        <ion-label aria-hidden="true">Delete</ion-label>
      </button>
    );
  }

  private renderNotes() {
    const classElement: string | undefined = `wider-devices ion-activatable ${this.slide ? '' : 'hidden'}`;

    return (
      <button onClick={() => this.openNotes()} aria-label="Notes" disabled={store.state.deckBusy} class={classElement} tabindex={this.slide ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/create.svg"></ion-icon>
        <ion-label aria-hidden="true">Notes</ion-label>
      </button>
    );
  }

  private renderCopy() {
    const displayed: boolean = this.slide || this.shape !== undefined;
    const classSlide: string | undefined = `wider-devices ion-activatable ${displayed ? '' : 'hidden'}`;

    return (
      <button onClick={() => this.clone()} aria-label="Copy" disabled={store.state.deckBusy} class={classSlide} tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/copy.svg"></ion-icon>
        <ion-label aria-hidden="true">Copy</ion-label>
      </button>
    );
  }

  private renderStyle() {
    return (
      <button onClick={() => this.openStyle()} aria-label="Style" class="ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/brush.svg"></ion-icon>
        <ion-label aria-hidden="true">Style</ion-label>
      </button>
    );
  }

  private renderEdit() {
    const displayed: boolean = this.slide && this.isSlideEditable();
    const classSlide: string | undefined = `ion-activatable${displayed ? '' : ' hidden'}`;

    return (
      <button
        onClick={() =>
          this.slideNodeName === 'deckgo-slide-poll'
            ? this.openEditPollSlide()
            : this.slideNodeName === 'deckgo-slide-youtube'
            ? this.openEditModalSlide('app-youtube', this.updateYoutube)
            : this.slideNodeName === 'deckgo-slide-playground'
            ? this.openEditModalSlide('app-playground', this.updatePlayground)
            : this.slideDemo
            ? this.openEditModalSlide('app-demo', this.updateSlideDemo)
            : this.openEditSlide()
        }
        aria-label="Edit slide options"
        class={classSlide}
        tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/settings.svg"></ion-icon>
        <ion-label aria-hidden="true">Options</ion-label>
      </button>
    );
  }

  private renderTransform() {
    const displayed: boolean = !this.slide && this.shape === undefined;
    const classToggle: string | undefined = `ion-activatable${displayed ? '' : ' hidden'}`;

    return (
      <button aria-label="Transform" onClick={() => this.openTransform()} class={classToggle} tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/color-wand.svg"></ion-icon>
        <ion-label aria-hidden="true">Transform</ion-label>
      </button>
    );
  }

  private renderAspectRatio() {
    const displayed: boolean = this.slideNodeName === 'deckgo-slide-aspect-ratio';
    const classSlide: string | undefined = `ion-activatable${displayed ? '' : ' hidden'}`;

    return [
      <button onClick={() => this.appendText()} aria-label="Add a text" class={classSlide} tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/text.svg"></ion-icon>
        <ion-label aria-hidden="true">Add text</ion-label>
      </button>,
      <button onClick={() => this.openShape('app-shape')} aria-label="Add a shape" class={classSlide} tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/shapes.svg"></ion-icon>
        <ion-label aria-hidden="true">Add shape</ion-label>
      </button>,
      <button onClick={() => this.openShape('app-image-element')} aria-label="Add an image" class={`wider-devices ${classSlide}`} tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/images.svg"></ion-icon>
        <ion-label aria-hidden="true">Add image</ion-label>
      </button>,
    ];
  }

  private renderCodeOptions() {
    const classSlideCode: string | undefined = `ion-activatable${this.code ? '' : ' hidden'}`;

    return (
      <button onClick={() => this.openCode()} aria-label="Code options" class={classSlideCode} tabindex={this.code ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/settings.svg"></ion-icon>
        <ion-label aria-hidden="true">Options</ion-label>
      </button>
    );
  }
  private renderMathOptions() {
    const classSlideMath: string | undefined = `ion-activatable${this.math ? '' : ' hidden'}`;

    return (
      <button onClick={() => this.openMath()} aria-label="Math options" class={classSlideMath} tabindex={this.math ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/settings.svg"></ion-icon>
        <ion-label aria-hidden="true">Options</ion-label>
      </button>
    );
  }

  private renderImages() {
    const classImage: string | undefined = `ion-activatable${this.image ? '' : ' hidden'}`;

    return (
      <button onClick={() => this.openImage()} aria-label="Image" class={classImage} tabindex={this.image ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/images.svg"></ion-icon>
        <ion-label aria-hidden="true">Image</ion-label>
      </button>
    );
  }

  private renderMore() {
    return (
      <button onClick={(e: UIEvent) => this.openMoreActions(e)} disabled={store.state.deckBusy} class="small-devices ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/ellipsis-vertical.svg"></ion-icon>
        <ion-label aria-hidden="true">More</ion-label>
      </button>
    );
  }
}
