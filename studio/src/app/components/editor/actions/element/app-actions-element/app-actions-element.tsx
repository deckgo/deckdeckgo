import {Component, Element, Event, EventEmitter, h, Listen, Method, Prop, State} from '@stencil/core';
import {modalController, OverlayEventDetail, popoverController} from '@ionic/core';

import {Subject, Subscription} from 'rxjs';
import {debounceTime} from 'rxjs/operators';

import {isFullscreen, isIOS, isMobile} from '@deckdeckgo/utils';

import {ImageHelper} from '../../../../../helpers/editor/image.helper';
import {ShapeHelper} from '../../../../../helpers/editor/shape.helper';

import {SlideSplitType} from '../../../../../models/data/slide';

import {ToggleSlotUtils} from '../../../../../utils/editor/toggle-slot.utils';
import {RevealSlotUtils} from '../../../../../utils/editor/reveal-slot.utils';
import {SlotType} from '../../../../../utils/editor/slot-type';
import {SlotUtils} from '../../../../../utils/editor/slot.utils';
import {AlignUtils, TextAlign} from '../../../../../utils/editor/align.utils';
import {ListUtils} from '../../../../../utils/editor/list.utils';

import {EditAction} from '../../../../../utils/editor/edit-action';
import {MoreAction} from '../../../../../utils/editor/more-action';
import {DemoAction} from '../../../../../utils/editor/demo-action';

import {BusyService} from '../../../../../services/editor/busy/busy.service';

@Component({
  tag: 'app-actions-element',
  styleUrl: 'app-actions-element.scss',
  shadow: false
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

  @State()
  private image: boolean = false;

  @State()
  private shape: boolean = false;

  @State()
  private align: TextAlign | undefined;

  @State()
  private list: SlotType.OL | SlotType.UL | undefined;

  @Event() private blockSlide: EventEmitter<boolean>;

  @Event() private slideDelete: EventEmitter<HTMLElement>;

  @Event() private slideDidChange: EventEmitter<HTMLElement>;
  @Event() private codeDidChange: EventEmitter<HTMLElement>;
  @Event() private imgDidChange: EventEmitter<HTMLElement>;
  @Event() private notesDidChange: EventEmitter<HTMLElement>;

  private subscription: Subscription;
  private busyService: BusyService;

  @State()
  private deckBusy: boolean = false;

  private elementResizeObserver: ResizeObserverConstructor;

  private moveToolbarSubscription: Subscription;
  private moveToolbarSubject: Subject<void> = new Subject();

  @Event() signIn: EventEmitter<void>;

  private imageHelper: ImageHelper;
  private shapeHelper: ShapeHelper;

  @Event() private resetted: EventEmitter<void>;

  constructor() {
    this.busyService = BusyService.getInstance();
  }

  async componentWillLoad() {
    this.subscription = this.busyService.watchDeckBusy().subscribe((busy: boolean) => {
      this.deckBusy = busy;
    });

    this.moveToolbarSubscription = this.moveToolbarSubject.pipe(debounceTime(250)).subscribe(async () => {
      await this.resizeSlideContent();
    });

    this.imageHelper = new ImageHelper(this.slideDidChange, this.blockSlide, this.signIn);
    this.shapeHelper = new ShapeHelper(this.slideDidChange, this.signIn);
  }

  async componentDidLoad() {
    this.initWindowResize();
  }

  async componentDidUnload() {
    if (this.subscription) {
      this.subscription.unsubscribe();
    }

    if (this.moveToolbarSubscription) {
      this.moveToolbarSubscription.unsubscribe();
    }

    this.removeWindowResize();
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', () => this.moveToolbarSubject.next(), {passive: true});
    }
  }

  private removeWindowResize() {
    if (window) {
      window.removeEventListener('resize', () => this.moveToolbarSubject.next(), true);
    }
  }

  @Listen('mouseInactivity', {target: 'document'})
  async inactivity(_$event: CustomEvent) {
    if (!isFullscreen()) {
      return;
    }

    await this.blurSelectedElement();
  }

  @Method()
  touch(element: HTMLElement, autoOpen: boolean = true): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.unSelect();
      await this.select(element, autoOpen);

      resolve();
    });
  }

  @Method()
  blurSelectedElement(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.selectedElement) {
        this.selectedElement.blur();
      }

      resolve();
    });
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

      if (autoOpen && this.isSlideAspectRatioEmpty(selected)) {
        await this.openShape();
      }

      this.blockSlide.emit(!this.slide);

      resolve();
    });
  }

  private isImgNotDefined(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === SlotType.IMG && !element.hasAttribute('img-src');
  }

  private isSlideAspectRatioEmpty(element: HTMLElement): boolean {
    return element && element.nodeName && this.slideNodeName === 'deckgo-slide-aspect-ratio' && !element.hasChildNodes();
  }

  @Method()
  unSelect(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.selectedElement) {
        this.selectedElement.removeEventListener('paste', this.cleanOnPaste, true);
        await this.detachMoveToolbarOnElement();

        await this.reset();
      }

      resolve();
    });
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

      if (element.hasAttribute('slot') && element.getAttribute('slot') !== 'code') {
        resolve(element);
        return;
      }

      const result: HTMLElement = await this.findSelectedElement(element.parentElement);

      resolve(result);
    });
  }

  private isElementSlide(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase().indexOf('deckgo-slide') > -1;
  }

  private isElementCode(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === SlotType.CODE;
  }

  private isElementMath(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === SlotType.MATH;
  }

  private isElementShape(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === SlotType.DRAG_RESIZE_ROTATE;
  }

  private isElementImage(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === SlotType.IMG;
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

  @Listen('pagerClick', {target: 'document'})
  async onPagerClick() {
    await this.reset();
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
      mode: isMobile() && !isIOS() ? 'md' : 'ios'
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

      if (this.deckBusy && this.slide) {
        resolve();
        return;
      }

      this.busyService.deckBusy(true);

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
    if (this.shape) {
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

      if (this.deckBusy || !this.shape) {
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

      if (this.deckBusy || !this.slide) {
        resolve();
        return;
      }

      this.busyService.deckBusy(true);

      this.slideCopy.emit(this.selectedElement);

      await this.reset();

      resolve();
    });
  }

  private async openSlotType() {
    if (this.slide) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-slot-type',
      componentProps: {
        selectedElement: this.selectedElement
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data && detail.data.type) {
        await this.toggleSlotType(detail.data.type);
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
        slideDidChange: this.slideDidChange
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu'
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

  private async openShape() {
    if (!this.slide || this.slideNodeName !== 'deckgo-slide-aspect-ratio') {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-shape',
      componentProps: {
        selectedElement: this.selectedElement
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu'
    });

    popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        await this.shapeHelper.appendShape(this.selectedElement, detail.data);
      }
    });

    await popover.present();
  }

  private async getImagePopover(): Promise<HTMLIonPopoverElement> {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-image-element',
      componentProps: {
        selectedElement: this.selectedElement,
        slide: this.slide,
        imgDidChange: this.imgDidChange
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu'
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
        slideDidChange: this.slideDidChange
      }
    });

    modal.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async openSingleAction($event: UIEvent, component: 'app-reveal' | 'app-align' | 'app-list') {
    if (this.slide) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: component,
      componentProps: {
        selectedElement: this.selectedElement
      },
      mode: 'ios',
      event: $event,
      cssClass: 'info'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data && component === 'app-reveal') {
        await this.toggleReveal(detail.data.reveal);
      } else if (detail.data && component === 'app-align') {
        await this.updateAlignAttribute(detail.data.align);
      } else if (detail.data && component === 'app-list') {
        await this.toggleList(detail.data.list);
      }
    });

    await popover.present();
  }

  private async openCode() {
    if (!this.code) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-code',
      componentProps: {
        selectedElement: this.selectedElement,
        codeDidChange: this.codeDidChange
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu'
    });

    await popover.present();
  }

  private async openEditYoutubeSlide() {
    if (this.slideNodeName !== 'deckgo-slide-youtube') {
      return;
    }

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-youtube',
      componentProps: {
        selectedElement: this.selectedElement
      }
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data && this.selectedElement) {
        await this.updateYoutube(detail.data);
      }

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async openEditDemoSlide() {
    if (!this.slideDemo) {
      return;
    }

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-demo',
      componentProps: {
        selectedElement: this.selectedElement,
      },
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data && this.selectedElement) {
        await this.updateSlideDemo(detail.data);
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
        selectedElement: this.selectedElement
      }
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

  private toggleSlotType(type: SlotType): Promise<void> {
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

  private toggleReveal(reveal: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || !this.selectedElement.parentElement) {
        resolve();
        return;
      }

      const element: HTMLElement = await RevealSlotUtils.toggleReveal(this.selectedElement, reveal);

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

  @Listen('colorDidChange', {target: 'document'})
  async onColorDidChange(_$event: CustomEvent) {
    await this.emitChange();
  }

  private emitChange(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.selectedElement.parentElement) {
        resolve();
        return;
      }

      // If not deck or slide, then parent is the container slide
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
      this.code = this.isElementCode(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element);
      this.image = this.isElementImage(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element);
      this.shape = this.isElementShape(element);

      this.align = await AlignUtils.getAlignment(element);

      this.list = await ListUtils.isElementList(element);

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
        this.selectedElement.addEventListener('focusout', () => this.moveToolbarSubject.next(), {passive: true});
      }

      resolve();
    });
  }

  private detachMoveToolbarOnElement(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (window && 'ResizeObserver' in window) {
        if (this.elementResizeObserver && this.selectedElement) {
          this.elementResizeObserver.unobserve(this.selectedElement);
          this.elementResizeObserver.disconnect;
        }
      } else {
        this.selectedElement.removeEventListener('focusout', () => this.moveToolbarSubject.next(), true);
      }

      resolve();
    });
  }

  private async openColor() {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-color',
      componentProps: {
        slide: this.slide,
        selectedElement: this.selectedElement
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: `popover-menu ${this.slideNodeName === 'deckgo-slide-poll' ? 'popover-menu-wide' : ''}`
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

  private updateYoutube(youtubeUrl: string): Promise<void> {
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
  }

  private async updateSlideDemo(demoAttr: DemoAction): Promise<void> {
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
  }

  private toggleList(destinationListType: SlotType.OL | SlotType.UL): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || !this.list) {
        resolve();
        return;
      }

      if (SlotUtils.isNodeRevealList(this.selectedElement)) {
        await this.updateRevealListAttribute(destinationListType);
      } else {
        await this.toggleSlotType(destinationListType);
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

  private async updateAlignAttribute(align: TextAlign): Promise<void> {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.textAlign = align;

    await this.emitChange();

    await this.reset();
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
      this.slideNodeName === 'deckgo-slide-author' ||
      this.slideDemo
    );
  }

  private async openMoreActions($event: UIEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-more-element-actions',
      componentProps: {
        notes: this.slide,
        copy: this.slide || this.shape,
        reveal: !this.hideReveal(),
        list: this.list !== undefined
      },
      event: $event,
      mode: 'ios'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        if (detail.data.action === MoreAction.NOTES) {
          await this.openNotes();
        } else if (detail.data.action === MoreAction.COPY) {
          await this.clone();
        } else if (detail.data.action === MoreAction.DELETE) {
          await this.confirmDeleteElement($event);
        } else if (detail.data.action === MoreAction.REVEAL) {
          await this.openSingleAction($event, 'app-reveal');
        } else if (detail.data.action === MoreAction.LIST) {
          await this.openSingleAction($event, 'app-list');
        }
      }
    });

    await popover.present();
  }

  private hideReveal(): boolean {
    return this.slide || this.code || this.math || this.shape || this.slideNodeName === 'deckgo-slide-youtube';
  }

  render() {
    return (
      <ion-toolbar>
        <ion-buttons slot="start">
          {this.renderEdit()}
          {this.renderShapes()}
          {this.renderColor()}
          {this.renderReveal()}
          {this.renderAlign()}
          {this.renderList()}
          {this.renderImages()}
          {this.renderCodeOptions()}
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
      <ion-tab-button
        onClick={($event: UIEvent) => this.confirmDeleteElement($event)}
        aria-label="Delete"
        color="primary"
        mode="md"
        disabled={this.deckBusy && this.slide}
        class="wider-devices">
        <ion-icon src="/assets/icons/ionicons/trash-bin.svg"></ion-icon>
        <ion-label>Delete</ion-label>
      </ion-tab-button>
    );
  }

  private renderNotes() {
    const classElement: string | undefined = `wider-devices ${this.slide ? '' : 'hidden'}`;

    return (
      <ion-tab-button onClick={() => this.openNotes()} aria-label="Notes" color="primary" mode="md" disabled={this.deckBusy} class={classElement}>
        <ion-icon src="/assets/icons/ionicons/create.svg"></ion-icon>
        <ion-label>Notes</ion-label>
      </ion-tab-button>
    );
  }

  private renderCopy() {
    const classSlide: string | undefined = `wider-devices ${this.slide || this.shape ? '' : 'hidden'}`;

    return (
      <ion-tab-button onClick={() => this.clone()} aria-label="Copy" color="primary" mode="md" disabled={this.deckBusy} class={classSlide}>
        <ion-icon src="/assets/icons/ionicons/copy.svg"></ion-icon>
        <ion-label>Copy</ion-label>
      </ion-tab-button>
    );
  }

  private renderColor() {
    return (
      <ion-tab-button onClick={() => this.openColor()} aria-label="Color" color="primary" mode="md">
        <ion-icon src="/assets/icons/ionicons/color-palette.svg"></ion-icon>
        <ion-label>Color</ion-label>
      </ion-tab-button>
    );
  }

  private renderEdit() {
    const classSlide: string | undefined = this.slide && this.isSlideEditable() ? undefined : 'hidden';
    const classToggle: string | undefined = !this.slide && !this.shape ? undefined : 'hidden';

    return [
      <ion-tab-button
        onClick={() =>
          this.slideNodeName === 'deckgo-slide-poll'
            ? this.openEditPollSlide()
            : this.slideNodeName === 'deckgo-slide-youtube'
            ? this.openEditYoutubeSlide()
            : this.slideDemo
            ? this.openEditDemoSlide()
            : this.openEditSlide()
        }
        color="primary"
        aria-label="Edit slide options"
        mode="md"
        class={classSlide}>
        <ion-icon src="/assets/icons/ionicons/pencil.svg"></ion-icon>
        <ion-label>Options</ion-label>
      </ion-tab-button>,
      <ion-tab-button onClick={() => this.openSlotType()} aria-label="Toggle element type" color="primary" mode="md" class={classToggle}>
        <ion-icon src="/assets/icons/ionicons/add.svg"></ion-icon>
        <ion-label>Toggle</ion-label>
      </ion-tab-button>
    ];
  }

  private renderShapes() {
    const classSlide: string | undefined = this.slideNodeName === 'deckgo-slide-aspect-ratio' ? undefined : 'hidden';

    return [
      <ion-tab-button onClick={() => this.openShape()} color="primary" aria-label="Add a shape" mode="md" class={classSlide}>
        <ion-icon src="/assets/icons/ionicons/shapes.svg"></ion-icon>
        <ion-label>Add shape</ion-label>
      </ion-tab-button>
    ];
  }

  private renderCodeOptions() {
    const classSlideCode: string | undefined = this.code ? undefined : 'hidden';

    return (
      <ion-tab-button onClick={() => this.openCode()} aria-label="Code attributes" color="primary" mode="md" class={classSlideCode}>
        <ion-icon src="/assets/icons/ionicons/code.svg"></ion-icon>
        <ion-label>Attributes</ion-label>
      </ion-tab-button>
    );
  }

  private renderImages() {
    const classImage: string | undefined = this.image || this.slide ? undefined : 'hidden';

    return (
      <ion-tab-button onClick={() => this.openImage()} aria-label={this.slide ? 'Background' : 'Image'} color="primary" mode="md" class={classImage}>
        <ion-icon src="/assets/icons/ionicons/images.svg"></ion-icon>
        <ion-label>{this.slide ? 'Background' : 'Image'}</ion-label>
      </ion-tab-button>
    );
  }

  private renderReveal() {
    const classReveal: string | undefined = this.hideReveal() ? 'hidden wider-devices' : 'wider-devices';

    return (
      <ion-tab-button
        onClick={($event: UIEvent) => this.openSingleAction($event, 'app-reveal')}
        aria-label="Edit element animation"
        color="primary"
        mode="md"
        class={classReveal}>
        <ion-icon src="/assets/icons/album.svg"></ion-icon>
        <ion-label>Animation</ion-label>
      </ion-tab-button>
    );
  }

  private renderAlign() {
    const classAlign: string | undefined = this.align === undefined ? 'hidden' : undefined;

    return (
      <ion-tab-button
        onClick={($event: UIEvent) => this.openSingleAction($event, 'app-align')}
        aria-label="Edit element alignment"
        color="primary"
        mode="md"
        class={classAlign}>
        {this.align !== undefined ? (
          <ion-icon src={`/assets/icons/align-${this.align}.svg`}></ion-icon>
        ) : (
          <ion-icon src={`/assets/icons/align-left.svg`}></ion-icon>
        )}
        <ion-label>Alignment</ion-label>
      </ion-tab-button>
    );
  }

  private renderList() {
    const classList: string | undefined = this.list === undefined ? 'hidden wider-devices' : 'wider-devices';

    return (
      <ion-tab-button
        onClick={($event: UIEvent) => this.openSingleAction($event, 'app-list')}
        aria-label="Edit ordered or unordered list"
        color="primary"
        mode="md"
        class={classList}>
        <ion-icon src={this.list === SlotType.OL ? '/assets/icons/list-ol.svg' : '/assets/icons/ionicons/list.svg'}></ion-icon>
        <ion-label>List</ion-label>
      </ion-tab-button>
    );
  }

  private renderMore() {
    return (
      <ion-tab-button onClick={(e: UIEvent) => this.openMoreActions(e)} disabled={this.deckBusy} color="primary" class="small-devices" mode="md">
        <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg"></ion-icon>
        <ion-label>More</ion-label>
      </ion-tab-button>
    );
  }
}
