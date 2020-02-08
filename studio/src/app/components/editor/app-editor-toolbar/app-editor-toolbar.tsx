import {Component, Element, Event, EventEmitter, h, Listen, Method, State} from '@stencil/core';
import {modalController, OverlayEventDetail, popoverController} from '@ionic/core';

import {Subject, Subscription} from 'rxjs';
import {debounceTime} from 'rxjs/operators';

import {isFullscreen, isIOS, isMobile} from '@deckdeckgo/utils';

import {ImageHelper} from '../../../helpers/editor/image.helper';

import {ToggleSlotUtils} from '../../../utils/editor/toggle-slot.utils';
import {RevealSlotUtils} from '../../../utils/editor/reveal-slot.utils';
import {SlotType} from '../../../utils/editor/slot-type';
import {SlotUtils} from '../../../utils/editor/slot.utils';

import {EditAction} from '../../../utils/editor/edit-action';

import {BusyService} from '../../../services/editor/busy/busy.service';

@Component({
  tag: 'app-editor-toolbar',
  styleUrl: 'app-editor-toolbar.scss',
  shadow: false
})
export class AppEditorToolbar {
  @Element() el: HTMLElement;

  @State()
  private displayed: boolean = false;

  private selectedElement: HTMLElement;

  @State()
  private slide: boolean = false;

  @State()
  private code: boolean = false;

  @State()
  private qrCode: boolean = false;

  @State()
  private chart: boolean = false;

  @State()
  private youtube: boolean = false;

  @State()
  private poll: boolean = false;

  @State()
  private author: boolean = false;

  @State()
  private image: boolean = false;

  @State()
  private list: SlotType;

  @Event() private blockSlide: EventEmitter<boolean>;

  @Event() private slideDelete: EventEmitter<HTMLElement>;

  @Event() private slideDidChange: EventEmitter<HTMLElement>;
  @Event() private codeDidChange: EventEmitter<HTMLElement>;
  @Event() private imgDidChange: EventEmitter<HTMLElement>;
  @Event() private notesDidChange: EventEmitter<HTMLElement>;

  @Event() private slideCopy: EventEmitter<HTMLElement>;

  @Event() private elementFocus: EventEmitter<HTMLElement>;

  private subscription: Subscription;
  private busyService: BusyService;

  @State()
  private deckBusy: boolean = false;

  private elementResizeObserver: ResizeObserverConstructor;

  private moveToolbarSubscription: Subscription;
  private moveToolbarSubject: Subject<void> = new Subject();

  @Event() signIn: EventEmitter<void>;

  private imageHelper: ImageHelper;

  constructor() {
    this.busyService = BusyService.getInstance();
  }

  async componentWillLoad() {
    this.subscription = this.busyService.watchDeckBusy().subscribe((busy: boolean) => {
      this.deckBusy = busy;
    });

    this.moveToolbarSubscription = this.moveToolbarSubject.pipe(debounceTime(250)).subscribe(async () => {
      await this.moveToolbar();
      await this.resizeSlideContent();
    });

    this.imageHelper = new ImageHelper(this.slideDidChange, this.blockSlide, this.signIn);
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
  async inactivity($event: CustomEvent) {
    if (!isFullscreen()) {
      return;
    }

    this.displayed = $event.detail;

    await this.blurSelectedElement();
  }

  @Method()
  touch(element: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.unSelect();
      await this.select(element);

      resolve(null);
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

  private select(element: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const selected: HTMLElement = await this.findSelectedElement(element);

      if (!selected) {
        this.blockSlide.emit(false);
        resolve();
        return;
      }

      await this.initSelectedElement(selected);

      // In case of slot deckgo-lazy-img, if user doesn't have yet defined a src for the image, we display the image picker/popover first instead of the toolbar
      if (this.isImgNotDefined(element)) {
        await this.openImage();
      }

      await this.displayToolbar(selected);

      this.blockSlide.emit(!this.slide);

      resolve();
    });
  }

  private isImgNotDefined(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === SlotType.IMG && !element.hasAttribute('img-src');
  }

  @Method()
  unSelect(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.selectedElement) {
        this.selectedElement.removeEventListener('paste', this.cleanOnPaste, true);
        await this.detachMoveToolbarOnElement();

        await this.hideToolbar();
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
    return element && element.nodeName && element.nodeName.toLowerCase() === 'deckgo-highlight-code';
  }

  private isElementYoutubeSlide(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === 'deckgo-slide-youtube';
  }

  private isElementQRCodeSlide(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === 'deckgo-slide-qrcode';
  }

  private isElementAuthorSlide(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === 'deckgo-slide-author';
  }

  private isElementChartSlide(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === 'deckgo-slide-chart';
  }

  private isElementPollSlide(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() === 'deckgo-slide-poll';
  }

  private isElementList(element: HTMLElement): SlotType {
    if (!SlotUtils.isNodeList(element)) {
      return undefined;
    }

    if (SlotUtils.isNodeRevealList(element)) {
      return element && element.getAttribute('list-tag') === SlotType.UL ? SlotType.UL : SlotType.OL;
    } else {
      return element && element.nodeName && element.nodeName.toLowerCase() === SlotType.OL ? SlotType.OL : SlotType.UL;
    }
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

  private displayToolbar(element: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!element) {
        resolve();
        return;
      }

      await this.moveToolbar();

      this.displayed = true;

      resolve();
    });
  }

  private moveToolbar(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      const toolbar: HTMLElement = this.el.querySelector('div.editor-toolbar');

      if (!toolbar) {
        resolve();
        return;
      }

      await this.setToolbarPosition(this.selectedElement, toolbar, 0);

      resolve();
    });
  }

  private setToolbarPosition(src: HTMLElement, applyTo: HTMLElement, offsetWidth: number): Promise<void> {
    return new Promise<void>((resolve) => {
      // Selected element
      const width: number = src.clientWidth > 200 ? src.clientWidth : 200;

      const rect: ClientRect = src.getBoundingClientRect();
      const left: number = rect && rect.left > 0 ? rect.left : 0;
      const top: number = rect && rect.top > 0 ? rect.top : 0;

      // The <main/> element in order to find the offset
      const mainPaneRect: ClientRect =
        applyTo.parentElement && applyTo.parentElement.parentElement ? applyTo.parentElement.parentElement.getBoundingClientRect() : null;
      const extraLeft: number = mainPaneRect && mainPaneRect.left > 0 ? mainPaneRect.left : 0;
      const extraTop: number = mainPaneRect ? mainPaneRect.top : 0;

      // Set top position
      const topPosition: string = `${top - extraTop > 0 ? top - extraTop : 0}px`;
      applyTo.style.top = this.slide ? `calc(${topPosition} + var(--editor-toolbar-padding, 0px))` : `${topPosition}`;

      const windowWidth: number = window.innerWidth | screen.width;

      const leftStandardPosition: number = left + offsetWidth - extraLeft;

      // Set left position
      const leftPosition: string = `${leftStandardPosition + width > windowWidth ? windowWidth - width : leftStandardPosition}px`;
      applyTo.style.left = this.slide ? `calc(${leftPosition} + var(--editor-toolbar-padding, 0px))` : `${leftPosition}`;

      // If not slide or deck selected, move a bit the toolbar
      if (!this.slide) {
        applyTo.style.transform = 'translate(0, -2.4rem)';
      } else {
        applyTo.style.transform = 'translate(0,0)';
      }

      // Set a width in order to align right the delete button
      applyTo.style.width = this.slide ? `calc(${width}px - var(--editor-toolbar-padding, 0px) - var(--editor-toolbar-padding, 0px))` : `${width}px`;

      resolve();
    });
  }

  @Listen('pagerClick', {target: 'document'})
  async onPagerClick() {
    await this.hideToolbar();
  }

  @Method()
  hideToolbar(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.displayed = false;
      await this.initSelectedElement(null);

      resolve();
    });
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

      await this.hideToolbar();

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

      await this.hideToolbar();

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
    if (!this.slide || (!this.qrCode && !this.chart && !this.author)) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-edit-slide',
      componentProps: {
        selectedElement: this.selectedElement,
        qrCode: this.qrCode,
        chart: this.chart,
        author: this.author,
        slideDidChange: this.slideDidChange
      },
      mode: 'md',
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

  private async openEditPollSlide() {
    if (!this.slide || !this.poll) {
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

  private async openReveal() {
    if (this.slide) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-reveal',
      componentProps: {
        selectedElement: this.selectedElement
      },
      mode: 'md',
      cssClass: 'popover-menu'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        await this.toggleReveal(detail.data.reveal);
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
      cssClass: 'popover-menu'
    });

    await popover.present();
  }

  private async openEditYoutubeSlide() {
    if (!this.youtube) {
      return;
    }

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-youtube',
      componentProps: {
        selectedElement: this.selectedElement
      }
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data && this.selectedElement && this.youtube) {
        await this.updateYoutube(detail.data);
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

      await this.hideToolbar();

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

      this.youtube = this.isElementYoutubeSlide(element);
      this.qrCode = this.isElementQRCodeSlide(element);
      this.chart = this.isElementChartSlide(element);
      this.poll = this.isElementPollSlide(element);
      this.author = this.isElementAuthorSlide(element);

      this.code = this.isElementCode(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element);
      this.image = this.isElementImage(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element);

      this.list = this.isElementList(element);

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
          await this.moveToolbar();

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
      cssClass: `popover-menu ${this.poll ? 'popover-menu-wide' : ''}`
    });

    await popover.present();
  }

  private async openImage() {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-image-slide',
      componentProps: {
        selectedElement: this.selectedElement,
        slide: this.slide,
        imgDidChange: this.imgDidChange
      },
      mode: 'md',
      cssClass: 'popover-menu'
    });

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
      if (!this.selectedElement || !this.youtube) {
        resolve();
        return;
      }

      if (!youtubeUrl || youtubeUrl === undefined || youtubeUrl === '') {
        resolve();
        return;
      }

      // Just in case
      if (!this.isElementYoutubeSlide(this.selectedElement)) {
        resolve();
        return;
      }

      this.selectedElement.setAttribute('src', youtubeUrl);
      this.slideDidChange.emit(this.selectedElement);

      resolve();
    });
  }

  private toggleList(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || !this.list) {
        resolve();
        return;
      }

      const destinationListType: SlotType = this.list === SlotType.UL ? SlotType.OL : SlotType.UL;

      if (SlotUtils.isNodeRevealList(this.selectedElement)) {
        await this.updateRevealListAttribute(destinationListType);
      } else {
        await this.toggleSlotType(destinationListType);
      }

      this.list = destinationListType;

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

      await this.hideToolbar();

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

  render() {
    return [
      <div class={this.displayed ? 'editor-toolbar displayed' : 'editor-toolbar'}>
        {this.renderEdit()}
        {this.renderReveal()}
        {this.renderColor()}
        {this.renderList()}
        {this.renderImages()}
        {this.renderCodeOptions()}

        <div class="editor-toolbar-edit">
          {this.renderNotes()}
          {this.renderCopy()}
          {this.renderDelete()}
        </div>
      </div>
    ];
  }

  private renderDelete() {
    return (
      <a onClick={($event: UIEvent) => this.confirmDeleteElement($event)} title="Delete" class={this.deckBusy && this.slide ? 'disabled' : ''}>
        <ion-icon name="trash"></ion-icon>
      </a>
    );
  }

  private renderNotes() {
    if (!this.slide) {
      return undefined;
    } else {
      return (
        <a onClick={() => this.openNotes()} title="Notes" class={this.deckBusy ? 'disabled' : ''}>
          <ion-icon src="/assets/icons/notes.svg"></ion-icon>
        </a>
      );
    }
  }

  private renderCopy() {
    if (!this.slide) {
      return undefined;
    } else {
      return (
        <a onClick={() => this.cloneSlide()} title="Copy" class={this.deckBusy ? 'disabled' : ''}>
          <ion-icon name="copy"></ion-icon>
        </a>
      );
    }
  }

  private renderColor() {
    return (
      <a onClick={() => this.openColor()} title="Color">
        <ion-icon name="color-fill"></ion-icon>
      </a>
    );
  }

  private renderEdit() {
    if (this.slide) {
      if (!this.qrCode && !this.chart && !this.poll && !this.youtube && !this.author) {
        return undefined;
      }

      return (
        <a
          onClick={() => (this.poll ? this.openEditPollSlide() : this.youtube ? this.openEditYoutubeSlide() : this.openEditSlide())}
          title="Edit slide options">
          <ion-icon src="/assets/icons/ionicons/md-create.svg"></ion-icon>
        </a>
      );
    } else {
      return (
        <a onClick={() => this.openSlotType()} title="Toggle element type">
          <ion-icon src="/assets/icons/ionicons/md-add.svg"></ion-icon>
        </a>
      );
    }
  }

  private renderCodeOptions() {
    if (!this.code) {
      return undefined;
    } else {
      return (
        <a onClick={() => this.openCode()} title="Code attributes">
          <ion-icon name="code"></ion-icon>
        </a>
      );
    }
  }

  private renderImages() {
    if (!this.image && !this.slide) {
      return undefined;
    } else {
      return (
        <a onClick={() => this.openImage()} title={this.slide ? 'Background' : 'Image'}>
          <ion-icon name="images"></ion-icon>
        </a>
      );
    }
  }

  private renderReveal() {
    if (this.slide || this.code || this.youtube) {
      return undefined;
    } else {
      return (
        <a onClick={() => this.openReveal()} title="Edit element animation">
          <ion-icon src="/assets/icons/album.svg"></ion-icon>
        </a>
      );
    }
  }

  private renderList() {
    if (this.slide || !this.list) {
      return undefined;
    } else if (this.list === SlotType.OL) {
      return (
        <a onClick={() => this.toggleList()} title="Toggle to an unordered list">
          <ion-icon src="/assets/icons/ionicons/ios-list.svg"></ion-icon>
        </a>
      );
    } else {
      return (
        <a onClick={() => this.toggleList()} title="Toggle to an ordered list">
          <ion-icon src="/assets/icons/list-ol.svg"></ion-icon>
        </a>
      );
    }
  }
}
