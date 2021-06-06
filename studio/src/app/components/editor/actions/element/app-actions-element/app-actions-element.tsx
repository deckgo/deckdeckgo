import {Component, Element, Event, EventEmitter, h, JSX, Listen, Method, Prop, State} from '@stencil/core';
import {modalController, OverlayEventDetail, popoverController} from '@ionic/core';

import {debounce, isFullscreen, isIOS, isMobile} from '@deckdeckgo/utils';
import {isSlide} from '@deckdeckgo/deck-utils';

import store from '../../../../../stores/busy.store';
import i18n from '../../../../../stores/i18n.store';
import undoRedoStore from '../../../../../stores/undo-redo.store';

import {ImageHelper} from '../../../../../helpers/editor/image.helper';
import {ShapeHelper} from '../../../../../helpers/editor/shape.helper';

import {ToggleSlotUtils} from '../../../../../utils/editor/toggle-slot.utils';
import {RevealSlotUtils} from '../../../../../utils/editor/reveal-slot.utils';
import {SlotUtils} from '../../../../../utils/editor/slot.utils';
import {SelectedElementUtils} from '../../../../../utils/editor/selected-element.utils';

import {SlotType} from '../../../../../types/editor/slot-type';
import {EditAction} from '../../../../../types/editor/edit-action';
import {MoreAction} from '../../../../../types/editor/more-action';
import {DemoAction} from '../../../../../types/editor/demo-action';
import {PlaygroundAction} from '../../../../../types/editor/playground-action';
import {SelectedElement} from '../../../../../types/editor/selected-element';

import {SlideScope} from '../../../../../models/data/slide';
import {InitTemplate} from '../../../../../utils/editor/create-slides.utils';
import {CloneSlideUtils} from '../../../../../utils/editor/clone-slide.utils';

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
  slideTransform: EventEmitter;

  @Prop()
  elementFocus: EventEmitter;

  @State()
  private selectedElement: SelectedElement | undefined;

  @Event() private blockSlide: EventEmitter<boolean>;

  @Event() private slideDelete: EventEmitter<HTMLElement>;

  @Event() private slideDidChange: EventEmitter<HTMLElement>;
  @Event() private codeDidChange: EventEmitter<HTMLElement>;
  @Event() private mathDidChange: EventEmitter<HTMLElement>;
  @Event() private imgDidChange: EventEmitter<HTMLElement>;
  @Event() private notesDidChange: EventEmitter<HTMLElement>;

  private elementResizeObserver: ResizeObserver;

  private readonly debounceResizeSlideContent: () => void;

  @Event() signIn: EventEmitter<void>;

  private imageHelper: ImageHelper;
  private shapeHelper: ShapeHelper;

  @Event() private resetted: EventEmitter<void>;

  private observeElementMutations = () => {
    if (undoRedoStore.state.elementInnerHTML === undefined) {
      return;
    }

    undoRedoStore.state.undo.push({type: 'input', target: this.selectedElement.element, data: {innerHTML: undoRedoStore.state.elementInnerHTML}});

    undoRedoStore.state.elementInnerHTML = this.selectedElement.element.innerHTML;
  };

  private observer: MutationObserver = new MutationObserver(this.observeElementMutations);

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
    if (!this.selectedElement?.element?.parentElement || !$event) {
      return;
    }

    const element: HTMLElement = await RevealSlotUtils.toggleReveal(this.selectedElement.element, $event.detail);

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
  async touch(element: HTMLElement | undefined, autoOpen: boolean = true) {
    await this.unSelect();
    await this.select(element, autoOpen);

    if (!element) {
      return;
    }

    element.focus();

    if (this.selectedElement?.type === 'element') {
      this.observer.takeRecords();
      this.observer.observe(this.selectedElement.element, {attributes: true, childList: true, subtree: true});

      undoRedoStore.state.elementInnerHTML = this.selectedElement.element.innerHTML;
      return;
    }

    this.observer.disconnect();

    undoRedoStore.state.elementInnerHTML = undefined;
  }

  @Method()
  async blurSelectedElement() {
    this.selectedElement?.element?.blur();
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

      this.blockSlide.emit(this.selectedElement?.type === 'element');

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

    await this.detachMoveToolbarOnElement();

    await this.reset();
  }

  private findSelectedElement(element: HTMLElement): Promise<HTMLElement> {
    return new Promise<HTMLElement>(async (resolve) => {
      if (!element || !element.parentElement) {
        resolve(element);
        return;
      }

      if (isSlide(element)) {
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

  @Method()
  async reset() {
    await this.initSelectedElement(undefined);

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

      if (store.state.deckBusy && this.selectedElement?.type === 'slide') {
        resolve();
        return;
      }

      store.state.deckBusy = true;

      if (this.selectedElement.type === 'slide') {
        this.slideDelete.emit(this.selectedElement.element);
      } else {
        const parent: HTMLElement = this.selectedElement.element.parentElement;
        this.selectedElement.element.parentElement.removeChild(this.selectedElement.element);
        this.slideDidChange.emit(parent);

        await this.resizeSlideContent(parent);
      }

      await this.reset();

      resolve();
    });
  }

  private async openCopyStyle($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-copy-style',
      componentProps: {
        selectedElement: this.selectedElement.element,
      },
      mode: 'ios',
      event: $event,
    });

    await popover.present();
  }

  private async clone() {
    if (this.selectedElement?.slot?.shape !== undefined) {
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

      if (store.state.deckBusy || this.selectedElement?.slot?.shape === undefined) {
        resolve();
        return;
      }

      await this.shapeHelper.cloneShape(this.selectedElement.element);

      resolve();
    });
  }

  private cloneSlide(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (store.state.deckBusy || this.selectedElement?.type === 'element') {
        resolve();
        return;
      }

      store.state.deckBusy = true;

      this.slideCopy.emit(this.selectedElement.element);

      await this.reset();

      resolve();
    });
  }

  private async openTransform() {
    if (this.selectedElement?.type === 'slide' && !this.selectedElement?.slide?.fixed) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: this.selectedElement?.type === 'slide' ? 'app-transform-slide' : 'app-transform-element',
      componentProps: {
        selectedElement: this.selectedElement.element,
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data?.type) {
        await this.transformSlotType(detail.data.type);
      } else if (detail.data?.template) {
        await this.transformTemplate(detail.data.template);
      }
    });

    await popover.present();
  }

  private async openEditSlide() {
    if (
      this.selectedElement?.type === 'element' ||
      (!this.selectedElement?.slide?.qrCode &&
        !this.selectedElement?.slide?.chart &&
        !this.selectedElement?.slide?.author &&
        this.selectedElement?.slide?.scope === SlideScope.DEFAULT)
    ) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-edit-slide',
      componentProps: {
        selectedElement: this.selectedElement,
        slideDidChange: this.slideDidChange,
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        if (detail.data.action === EditAction.DELETE_LOGO) {
          await this.deleteLogo();
        } else if (detail.data.action === EditAction.OPEN_CUSTOM_LOGO) {
          await this.imageHelper.openCustomModalRestricted(
            this.selectedElement.element,
            this.selectedElement?.type === 'slide',
            false,
            'app-custom-images',
            detail.data.action
          );
        } else if (detail.data.action === EditAction.OPEN_DATA) {
          await this.imageHelper.openCustomModalRestricted(
            this.selectedElement.element,
            this.selectedElement?.type === 'slide',
            false,
            'app-custom-data',
            detail.data.action
          );
        }
      }

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await popover.present();
  }

  private async openShape(component: 'app-shape' | 'app-image-element') {
    if (this.selectedElement?.type === 'element' || !this.selectedElement?.slide?.aspectRatio) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: component,
      componentProps: {
        selectedElement: this.selectedElement.element,
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        await this.shapeHelper.appendShape(
          this.selectedElement.element,
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
    await this.shapeHelper.appendText(this.selectedElement.element);
  }

  private async getImagePopover(): Promise<HTMLIonPopoverElement> {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-image-element',
      componentProps: {
        selectedElement: this.selectedElement.element,
        slide: this.selectedElement?.type === 'slide',
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    return popover;
  }

  private async openEditPollSlide() {
    if (this.selectedElement?.type === 'element' || this.selectedElement?.slide?.poll) {
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
    if (!this.selectedElement?.slot?.code) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-code',
      componentProps: {
        selectedElement: this.selectedElement.element,
        codeDidChange: this.codeDidChange,
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu',
    });

    await popover.present();
  }
  private async openMath() {
    if (!this.selectedElement?.slot?.math) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-math',
      componentProps: {
        selectedElement: this.selectedElement.element,
        mathDidChange: this.mathDidChange,
      },
      mode: 'ios',
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
        selectedElement: this.selectedElement.element,
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
        selectedElement: this.selectedElement.element,
      },
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data && this.selectedElement) {
        this.notesDidChange.emit(this.selectedElement.element);
      }

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async transformSlotType(type: SlotType) {
    if (!this.selectedElement || !this.selectedElement.element.parentElement) {
      return;
    }

    const element: HTMLElement = await ToggleSlotUtils.toggleSlotType(this.selectedElement.element, type);

    await this.replaceSlot(element);
  }

  private async transformTemplate(template: InitTemplate) {
    if (!this.selectedElement || !this.selectedElement.slide?.fixed) {
      return;
    }

    const slide: JSX.IntrinsicElements = await CloneSlideUtils.toggleTemplate(this.selectedElement.element, template);

    // Catch event when slide is parsed and then persist it to the database
    document.addEventListener(
      'slideDidLoad',
      async ($event: CustomEvent) => {
        const slide: HTMLElement = $event.target as HTMLElement;

        this.slideDidChange.emit(slide);

        await this.initSelectedElement(slide);
      },
      {once: true}
    );

    this.slideTransform.emit(slide);
  }

  private async replaceSlot(element: HTMLElement) {
    if (!this.selectedElement || !this.selectedElement.element.parentElement || !element) {
      return;
    }

    this.selectedElement.element.parentElement.replaceChild(element, this.selectedElement.element);

    await this.initSelectedElement(element);

    await this.emitChange();

    await this.resizeSlideContent();

    await this.reset();
  }

  private emitChange(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.selectedElement.element.parentElement) {
        resolve();
        return;
      }

      // If not slide, then parent is the container slide
      this.slideDidChange.emit(this.selectedElement?.type === 'slide' ? this.selectedElement.element : this.selectedElement.element.parentElement);

      resolve();
    });
  }

  private async initSelectedElement(element: HTMLElement | undefined) {
    // If needed, remove highlight on previous element
    if (!element && this.selectedElement) {
      await this.highlightElement(false);
    }

    this.selectedElement = element
      ? {
          element,
          ...(SelectedElementUtils.initDescription(element) as SelectedElement),
        }
      : undefined;

    if (element) {
      await this.attachResizeSlideContent();

      await this.highlightElement(true);

      this.elementFocus.emit(element);
    }
  }

  private highlightElement(highlight: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      // No highlight on deck
      if (!this.selectedElement || this.selectedElement?.type === 'slide') {
        resolve();
        return;
      }

      if (highlight) {
        this.selectedElement.element.setAttribute('highlighted', '');
      } else {
        this.selectedElement.element.removeAttribute('highlighted');
      }

      resolve();
    });
  }

  private async attachResizeSlideContent() {
    if (!this.selectedElement) {
      return;
    }

    if (window && 'ResizeObserver' in window) {
      await this.detachMoveToolbarOnElement();

      this.elementResizeObserver = new ResizeObserver(async (entries) => {
        if (entries && entries.length > 0 && entries[0].target && entries[0].target.nodeName && !isSlide(entries[0].target as HTMLElement)) {
          await this.resizeSlideContent();
        }
      });

      this.elementResizeObserver.observe(this.selectedElement.element);
    } else {
      // Fallback, better  than nothing. It won't place the toolbar if the size on enter or delete  but at least if the content change like if list is toggled
      this.selectedElement.element.addEventListener('focusout', () => this.debounceResizeSlideContent(), {passive: true});
    }
  }

  private detachMoveToolbarOnElement(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (window && 'ResizeObserver' in window) {
        if (this.elementResizeObserver && this.selectedElement) {
          this.elementResizeObserver.unobserve(this.selectedElement.element);
          this.elementResizeObserver.disconnect();
        }
      } else {
        this.selectedElement.element.removeEventListener('focusout', () => this.debounceResizeSlideContent(), true);
      }

      resolve();
    });
  }

  private async openStyle() {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-element-style',
      componentProps: {
        selectedElement: this.selectedElement,
        imgDidChange: this.imgDidChange,
        imageHelper: this.imageHelper,
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: `popover-menu ${this.selectedElement?.slide?.poll ? 'popover-menu-wide' : ''}`,
    });

    await popover.present();
  }

  private async openImage() {
    const popover: HTMLIonPopoverElement = await this.getImagePopover();

    popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        await this.imageHelper.imageAction(this.selectedElement.element, this.selectedElement?.type === 'slide', false, detail.data);
      }
    });

    await popover.present();
  }

  private async deleteLogo() {
    await this.imageHelper.deleteSlideAttributeImgSrc(this.selectedElement.element);
  }

  private updateYoutube = (youtubeUrl: string): Promise<void> => {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || !this.selectedElement?.slide?.youtube) {
        resolve();
        return;
      }

      if (!youtubeUrl || youtubeUrl === undefined || youtubeUrl === '') {
        resolve();
        return;
      }

      this.selectedElement.element.setAttribute('src', youtubeUrl);
      this.slideDidChange.emit(this.selectedElement.element);

      resolve();
    });
  };

  private updatePlayground = async (playground: PlaygroundAction) => {
    if (!this.selectedElement || !this.selectedElement?.slide?.playground) {
      return;
    }

    if (!playground) {
      return;
    }

    if (!playground.src || playground.src === undefined || playground.src === '') {
      return;
    }

    this.selectedElement.element.setAttribute('src', playground.src);

    if (playground.theme) {
      this.selectedElement.element.setAttribute('theme', playground.theme);
    } else {
      this.selectedElement.element.removeAttribute('theme');
    }

    this.slideDidChange.emit(this.selectedElement.element);
  };

  private updateSlideDemo = async (demoAttr: DemoAction): Promise<void> => {
    if (!this.selectedElement || !this.selectedElement?.slide?.demo) {
      return;
    }

    if (!demoAttr || !demoAttr.src || demoAttr.src === undefined || demoAttr.src === '') {
      return;
    }

    const demo: HTMLElement = this.selectedElement.element.querySelector('deckgo-demo');

    if (!demo) {
      return;
    }

    demo.setAttribute('src', demoAttr.src);
    demo.setAttribute('mode', demoAttr.mode);

    this.slideDidChange.emit(this.selectedElement.element);
  };

  private toggleList(destinationListType: SlotType.OL | SlotType.UL): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (SlotUtils.isNodeRevealList(this.selectedElement.element)) {
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

      this.selectedElement.element.setAttribute('list-tag', type);

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

      const element: HTMLElement = slideElement
        ? slideElement
        : this.selectedElement?.type === 'slide'
        ? this.selectedElement.element
        : this.selectedElement.element.parentElement;

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
      this.selectedElement?.slide?.qrCode ||
      this.selectedElement?.slide?.chart ||
      this.selectedElement?.slide?.poll ||
      this.selectedElement?.slide?.youtube ||
      this.selectedElement?.slide?.playground ||
      this.selectedElement?.slide?.author ||
      this.selectedElement?.slide?.demo ||
      this.selectedElement?.slide?.scope !== SlideScope.DEFAULT
    );
  }

  private async openMoreActions($event: UIEvent) {
    if (!$event) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-more-element-actions',
      componentProps: {
        notes: this.selectedElement?.type === 'slide',
        clone: this.selectedElement?.type === 'slide' || this.selectedElement?.slot?.shape !== undefined,
        images: this.selectedElement?.slide?.aspectRatio,
        transform: this.displayTransform(),
      },
      event: $event,
      mode: 'ios',
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        if (detail.data.action === MoreAction.NOTES) {
          await this.openNotes();
        } else if (detail.data.action === MoreAction.CLONE) {
          await this.clone();
        } else if (detail.data.action === MoreAction.DELETE) {
          await this.confirmDeleteElement($event);
        } else if (detail.data.action === MoreAction.IMAGES) {
          await this.openShape('app-image-element');
        } else if (detail.data.action === MoreAction.TRANSFORM) {
          await this.openTransform();
        }
      }
    });

    await popover.present();
  }

  private displayTransform() {
    return (this.selectedElement?.type === 'element' || this.selectedElement?.slide?.fixed) && this.selectedElement?.slot?.shape === undefined;
  }

  render() {
    return (
      <aside>
        <ion-buttons slot="start">
          {this.renderStyle()}
          {this.renderCopyStyle()}
          {this.renderEdit()}
          {this.renderAspectRatio()}
          {this.renderImages()}
          {this.renderCodeOptions()}
          {this.renderMathOptions()}
        </ion-buttons>

        <ion-buttons slot="end">
          {this.renderNotes()}
          {this.renderClone()}
          {this.renderTransform()}
          {this.renderDelete()}
          {this.renderMore()}
        </ion-buttons>
      </aside>
    );
  }

  private renderDelete() {
    return (
      <button
        onClick={($event: UIEvent) => this.confirmDeleteElement($event)}
        aria-label={i18n.state.editor.delete}
        disabled={store.state.deckBusy && this.selectedElement?.type === 'slide'}
        class="wider-devices ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/trash-bin.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.delete}</ion-label>
      </button>
    );
  }

  private renderNotes() {
    const classElement: string | undefined = `wider-devices ion-activatable ${this.selectedElement?.type === 'slide' ? '' : 'hidden'}`;

    return (
      <button
        onClick={() => this.openNotes()}
        aria-label={i18n.state.editor.notes}
        disabled={store.state.deckBusy}
        class={classElement}
        tabindex={this.selectedElement?.type === 'slide' ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/create.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.notes}</ion-label>
      </button>
    );
  }

  private renderClone() {
    const displayed: boolean = this.selectedElement?.type === 'slide' || this.selectedElement?.slot?.shape !== undefined;
    const classSlide: string | undefined = `wider-devices ion-activatable ${displayed ? '' : 'hidden'}`;

    return (
      <button onClick={() => this.clone()} aria-label={i18n.state.editor.copy} disabled={store.state.deckBusy} class={classSlide} tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/copy.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.copy}</ion-label>
      </button>
    );
  }

  private renderCopyStyle() {
    const displayed: boolean = this.selectedElement?.type === 'element' && this.selectedElement?.slot?.shape === undefined;
    const classSlide: string | undefined = `ion-activatable ${displayed ? '' : 'hidden'}`;

    return (
      <button
        onClick={($event: UIEvent) => this.openCopyStyle($event)}
        aria-label={i18n.state.editor.format}
        disabled={store.state.deckBusy}
        class={classSlide}
        tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/color-wand.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.format}</ion-label>
      </button>
    );
  }

  private renderStyle() {
    return (
      <button onClick={() => this.openStyle()} aria-label={i18n.state.editor.style} class="ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/brush.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.style}</ion-label>
      </button>
    );
  }

  private renderEdit() {
    const displayed: boolean = this.selectedElement?.type === 'slide' && this.isSlideEditable();
    const classSlide: string | undefined = `ion-activatable${displayed ? '' : ' hidden'}`;

    return (
      <button
        onClick={() =>
          this.selectedElement?.slide?.poll
            ? this.openEditPollSlide()
            : this.selectedElement?.slide?.youtube
            ? this.openEditModalSlide('app-youtube', this.updateYoutube)
            : this.selectedElement?.slide?.playground
            ? this.openEditModalSlide('app-playground', this.updatePlayground)
            : this.selectedElement?.slide?.demo
            ? this.openEditModalSlide('app-demo', this.updateSlideDemo)
            : this.selectedElement?.slide?.scope !== SlideScope.DEFAULT
            ? this.openEditSlide()
            : this.openEditSlide()
        }
        aria-label={i18n.state.editor.options}
        class={classSlide}
        tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/settings.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.options}</ion-label>
      </button>
    );
  }

  private renderTransform() {
    const displayed: boolean = this.displayTransform();
    const classToggle: string | undefined = `wider-devices ion-activatable${displayed ? '' : ' hidden'}`;

    return (
      <button aria-label={i18n.state.editor.transform} onClick={() => this.openTransform()} class={classToggle} tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/flask.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.transform}</ion-label>
      </button>
    );
  }

  private renderAspectRatio() {
    const displayed: boolean = this.selectedElement?.slide?.aspectRatio;
    const classSlide: string | undefined = `ion-activatable${displayed ? '' : ' hidden'}`;

    return [
      <button onClick={() => this.appendText()} aria-label={i18n.state.editor.add_text} class={classSlide} tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/text.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.add_text}</ion-label>
      </button>,
      <button onClick={() => this.openShape('app-shape')} aria-label={i18n.state.editor.add_shape} class={classSlide} tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/shapes.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.add_shape}</ion-label>
      </button>,
      <button
        onClick={() => this.openShape('app-image-element')}
        aria-label={i18n.state.editor.add_image}
        class={`wider-devices ${classSlide}`}
        tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/images.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.add_image}</ion-label>
      </button>,
    ];
  }

  private renderCodeOptions() {
    const classSlideCode: string | undefined = `ion-activatable${this.selectedElement?.slot?.code ? '' : ' hidden'}`;

    return (
      <button
        onClick={() => this.openCode()}
        aria-label={i18n.state.editor.options}
        class={classSlideCode}
        tabindex={this.selectedElement?.slot?.code ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/settings.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.options}</ion-label>
      </button>
    );
  }
  private renderMathOptions() {
    const classSlideMath: string | undefined = `ion-activatable${this.selectedElement?.slot?.math ? '' : ' hidden'}`;

    return (
      <button
        onClick={() => this.openMath()}
        aria-label={i18n.state.editor.options}
        class={classSlideMath}
        tabindex={this.selectedElement?.slot?.math ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/settings.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.options}</ion-label>
      </button>
    );
  }

  private renderImages() {
    const classImage: string | undefined = `ion-activatable${this.selectedElement?.slot?.image ? '' : ' hidden'}`;

    return (
      <button onClick={() => this.openImage()} aria-label={i18n.state.editor.image} class={classImage} tabindex={this.selectedElement?.slot?.image ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/images.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.image}</ion-label>
      </button>
    );
  }

  private renderMore() {
    return (
      <button onClick={(e: UIEvent) => this.openMoreActions(e)} disabled={store.state.deckBusy} class="small-devices ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/ellipsis-vertical.svg"></ion-icon>
        <ion-label aria-hidden="true">{i18n.state.editor.more}</ion-label>
      </button>
    );
  }
}
