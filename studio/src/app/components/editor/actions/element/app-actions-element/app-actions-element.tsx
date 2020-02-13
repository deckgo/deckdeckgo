import {Component, Element, Event, EventEmitter, h, Listen, Method, State, Prop} from '@stencil/core';
import {modalController, OverlayEventDetail, popoverController} from '@ionic/core';

import {Subject, Subscription} from 'rxjs';
import {debounceTime} from 'rxjs/operators';

import {isFullscreen, isIOS, isMobile} from '@deckdeckgo/utils';

import {ImageHelper} from '../../../../../helpers/editor/image.helper';

import {ToggleSlotUtils} from '../../../../../utils/editor/toggle-slot.utils';
import {RevealSlotUtils} from '../../../../../utils/editor/reveal-slot.utils';
import {SlotType} from '../../../../../utils/editor/slot-type';
import {SlotUtils} from '../../../../../utils/editor/slot.utils';

import {EditAction} from '../../../../../utils/editor/edit-action';

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

  private subscription: Subscription;
  private busyService: BusyService;

  @State()
  private deckBusy: boolean = false;

  private elementResizeObserver: ResizeObserverConstructor;

  private moveToolbarSubscription: Subscription;
  private moveToolbarSubject: Subject<void> = new Subject();

  @Event() signIn: EventEmitter<void>;

  private imageHelper: ImageHelper;

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
  touch(element: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.unSelect();
      await this.select(element);

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

      if (element.getAttribute('slot') && element.getAttribute('slot') !== 'code') {
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

  render() {
    return (
      <ion-toolbar>
        <ion-buttons slot="start">
          {this.renderEdit()}
          {this.renderReveal()}
          {this.renderColor()}
          {this.renderList()}
          {this.renderImages()}
          {this.renderCodeOptions()}
        </ion-buttons>

        <ion-buttons slot="end">
          {this.renderNotes()}
          {this.renderCopy()}
          {this.renderDelete()}
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
        disabled={this.deckBusy && this.slide}>
        <ion-icon name="trash-outline"></ion-icon>
        <ion-label>Delete</ion-label>
      </ion-tab-button>
    );
  }

  private renderNotes() {
    const classElement: string | undefined = this.slide ? undefined : 'hidden';

    return (
      <ion-tab-button onClick={() => this.openNotes()} aria-label="Notes" color="primary" mode="md" disabled={this.deckBusy} class={classElement}>
        <ion-icon name="create-outline"></ion-icon>
        <ion-label>Notes</ion-label>
      </ion-tab-button>
    );
  }

  private renderCopy() {
    const classSlide: string | undefined = this.slide ? undefined : 'hidden';

    return (
      <ion-tab-button onClick={() => this.cloneSlide()} aria-label="Copy" color="primary" mode="md" disabled={this.deckBusy} class={classSlide}>
        <ion-icon name="copy-outline"></ion-icon>
        <ion-label>Copy</ion-label>
      </ion-tab-button>
    );
  }

  private renderColor() {
    return (
      <ion-tab-button onClick={() => this.openColor()} aria-label="Color" color="primary" mode="md">
        <ion-icon name="color-palette-outline"></ion-icon>
        <ion-label>Color</ion-label>
      </ion-tab-button>
    );
  }

  private renderEdit() {
    const classSlide: string | undefined = this.slide && (this.qrCode || this.chart || this.poll || this.youtube || this.author) ? undefined : 'hidden';
    const classToggle: string | undefined = !this.slide ? undefined : ' hidden';

    return [
      <ion-tab-button
        onClick={() => (this.poll ? this.openEditPollSlide() : this.youtube ? this.openEditYoutubeSlide() : this.openEditSlide())}
        color="primary"
        aria-label="Edit slide options"
        mode="md"
        class={classSlide}>
        <ion-icon name="pencil-outline"></ion-icon>
        <ion-label>Options</ion-label>
      </ion-tab-button>,
      <ion-tab-button onClick={() => this.openSlotType()} aria-label="Toggle element type" color="primary" mode="md" class={classToggle}>
        <ion-icon src="/assets/icons/ionicons/add.svg"></ion-icon>
        <ion-label>Toggle</ion-label>
      </ion-tab-button>
    ];
  }

  private renderCodeOptions() {
    const classSlideCode: string | undefined = this.code ? undefined : 'hidden';

    return (
      <ion-tab-button onClick={() => this.openCode()} aria-label="Code attributes" color="primary" mode="md" class={classSlideCode}>
        <ion-icon name="code-outline"></ion-icon>
        <ion-label>Attributes</ion-label>
      </ion-tab-button>
    );
  }

  private renderImages() {
    const classImage: string | undefined = this.image || this.slide ? undefined : 'hidden';

    return (
      <ion-tab-button onClick={() => this.openImage()} aria-label={this.slide ? 'Background' : 'Image'} color="primary" mode="md" class={classImage}>
        <ion-icon name="image-outline"></ion-icon>
        <ion-label>{this.slide ? 'Background' : 'Image'}</ion-label>
      </ion-tab-button>
    );
  }

  private renderReveal() {
    const classReveal: string | undefined = this.slide || this.code || this.youtube ? 'hidden' : undefined;

    return (
      <ion-tab-button onClick={() => this.openReveal()} aria-label="Edit element animation" color="primary" mode="md" class={classReveal}>
        <ion-icon src="/assets/icons/album.svg"></ion-icon>
        <ion-label>Animation</ion-label>
      </ion-tab-button>
    );
  }

  private renderList() {
    const classListOL: string | undefined = this.list === SlotType.OL ? undefined : 'hidden';
    const classListUL: string | undefined = this.list === SlotType.UL ? undefined : 'hidden';

    return [
      <ion-tab-button onClick={() => this.toggleList()} aria-label="Toggle to an unordered list" color="primary" mode="md" class={classListUL}>
        <ion-icon src="/assets/icons/ionicons/list.svg"></ion-icon>
        <ion-label>Unordered list</ion-label>
      </ion-tab-button>,
      <ion-tab-button onClick={() => this.toggleList()} aria-label="Toggle to an ordered list" color="primary" mode="md" class={classListOL}>
        <ion-icon src="/assets/icons/list-ol.svg"></ion-icon>
        <ion-label>Ordered list</ion-label>
      </ion-tab-button>
    ];
  }
}
