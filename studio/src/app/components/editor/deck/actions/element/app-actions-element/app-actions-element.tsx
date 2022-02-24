import {isSlide} from '@deckdeckgo/deck-utils';
import {SlideScope} from '@deckdeckgo/editor';
import {SlotType, SlotUtils} from '@deckdeckgo/studio';
import {debounce, isFullscreen} from '@deckdeckgo/utils';
import type {OverlayEventDetail} from '@ionic/core';
import {modalController, popoverController} from '@ionic/core';
import {Component, Element, Event, EventEmitter, h, JSX, Listen, Method, Prop, State} from '@stencil/core';
import {ImageHelper} from '../../../../../../helpers/editor/image.helper';
import {ShapeHelper} from '../../../../../../helpers/editor/shape.helper';
import busyStore from '../../../../../../stores/busy.store';
import i18n from '../../../../../../stores/i18n.store';
import undoRedoStore from '../../../../../../stores/undo-redo.store';
import {DemoAction} from '../../../../../../types/editor/demo-action';
import {EditAction} from '../../../../../../types/editor/edit-action';
import {MoreAction} from '../../../../../../types/editor/more-action';
import {PlaygroundAction} from '../../../../../../types/editor/playground-action';
import {SelectedTarget} from '../../../../../../types/editor/selected-target';
import {CloneSlideUtils} from '../../../../../../utils/editor/clone-slide.utils';
import {InitTemplate} from '../../../../../../utils/editor/create-slides.utils';
import {RevealSlotUtils} from '../../../../../../utils/editor/reveal-slot.utils';
import {SelectedElementUtils} from '../../../../../../utils/editor/selected-element.utils';
import {ToggleSlotUtils} from '../../../../../../utils/editor/toggle-slot.utils';
import {AppIcon} from '../../../../../core/app-icon/app-icon';

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
  slideTransform: EventEmitter;

  @Prop()
  elementFocus: EventEmitter;

  @State()
  private selectedTarget: SelectedTarget | undefined;

  @Event({bubbles: true})
  private blockSlide: EventEmitter<boolean>;

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

    if (undoRedoStore.state.elementInnerHTML === this.selectedTarget.target.innerHTML) {
      return;
    }

    if (undoRedoStore.state.undo === undefined) {
      undoRedoStore.state.undo = [];
    }

    undoRedoStore.state.undo.push({
      type: 'input',
      target: this.selectedTarget.target,
      data: {innerHTML: undoRedoStore.state.elementInnerHTML}
    });

    undoRedoStore.state.elementInnerHTML = this.selectedTarget.target.innerHTML;
  };

  private observer: MutationObserver = new MutationObserver(this.observeElementMutations);

  constructor() {
    this.debounceResizeSlideContent = debounce(async () => {
      await this.resizeSlideContent();
    }, 250);
  }

  async componentWillLoad() {
    this.imageHelper = new ImageHelper(this.slideDidChange, this.blockSlide, this.signIn);
    this.shapeHelper = new ShapeHelper(this.slideDidChange);
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

    await this.blurSelectedTarget();
  }

  @Listen('optionsDidChange', {target: 'document'})
  async onOptionsDidChange() {
    await this.emitChange();
  }

  @Listen('toggleReveal', {target: 'document'})
  async onToggleReveal($event: CustomEvent<boolean>) {
    if (!this.selectedTarget?.target?.parentElement || !$event) {
      return;
    }

    const element: HTMLElement = await RevealSlotUtils.toggleReveal(this.selectedTarget.target, $event.detail);

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

    if (this.selectedTarget?.type === 'element') {
      this.observer.takeRecords();
      this.observer.observe(this.selectedTarget.target, {attributes: true, childList: true, subtree: true});

      undoRedoStore.state.elementInnerHTML = this.selectedTarget.target.innerHTML;
      return;
    }

    this.observer.disconnect();

    undoRedoStore.state.elementInnerHTML = undefined;
  }

  @Method()
  async blurSelectedTarget() {
    this.selectedTarget?.target?.blur();
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

      this.blockSlide.emit(this.selectedTarget?.type === 'element');

      resolve();
    });
  }

  private isImgNotDefined(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.IMG && !element.hasAttribute('img-src');
  }

  @Method()
  async unSelect() {
    if (!this.selectedTarget) {
      return;
    }

    await this.detachResizeSlideContent();

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
      component: 'app-delete',
      event: $event,
      mode: 'ios'
    });

    popover.onDidDismiss().then(async ({data}: OverlayEventDetail) => {
      if (data) {
        await this.deleteElement();
      }
    });

    await popover.present();
  }

  private deleteElement(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget) {
        resolve();
        return;
      }

      if (busyStore.state.busy && this.selectedTarget?.type === 'slide') {
        resolve();
        return;
      }

      busyStore.state.busy = true;

      if (this.selectedTarget.type === 'slide') {
        this.slideDelete.emit(this.selectedTarget.target);
      } else {
        const parent: HTMLElement = this.selectedTarget.target.parentElement;
        this.selectedTarget.target.parentElement.removeChild(this.selectedTarget.target);
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
        selectedTarget: this.selectedTarget.target
      },
      mode: 'ios',
      event: $event
    });

    await popover.present();
  }

  private async clone() {
    if (this.selectedTarget?.element?.shape !== undefined) {
      await this.cloneShape();
    } else {
      await this.cloneSlide();
    }
  }

  private cloneShape(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget) {
        resolve();
        return;
      }

      if (busyStore.state.busy || this.selectedTarget?.element?.shape === undefined) {
        resolve();
        return;
      }

      await this.shapeHelper.cloneShape(this.selectedTarget.target);

      resolve();
    });
  }

  private cloneSlide(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget) {
        resolve();
        return;
      }

      if (busyStore.state.busy || this.selectedTarget?.type === 'element') {
        resolve();
        return;
      }

      busyStore.state.busy = true;

      this.slideCopy.emit(this.selectedTarget.target);

      await this.reset();

      resolve();
    });
  }

  private async openTransform() {
    if (this.selectedTarget?.type === 'slide' && !this.selectedTarget?.slide?.fixed) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: this.selectedTarget?.type === 'slide' ? 'app-transform-slide' : 'app-transform-element',
      componentProps: {
        selectedTarget: this.selectedTarget.target
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu'
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
      this.selectedTarget?.type === 'element' ||
      (!this.selectedTarget?.slide?.qrCode &&
        !this.selectedTarget?.slide?.chart &&
        !this.selectedTarget?.slide?.author &&
        this.selectedTarget?.slide?.scope === SlideScope.DEFAULT)
    ) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-edit-slide',
      componentProps: {
        selectedTarget: this.selectedTarget,
        slideDidChange: this.slideDidChange
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu'
    });

    popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        if (detail.data.action === EditAction.DELETE_LOGO) {
          await this.deleteLogo();
        } else if (detail.data.action === EditAction.OPEN_CUSTOM_LOGO) {
          await this.imageHelper.openCustomModalRestricted(
            this.selectedTarget.target,
            this.selectedTarget?.type === 'slide',
            false,
            'app-storage-images',
            detail.data.action
          );
        } else if (detail.data.action === EditAction.OPEN_DATA) {
          await this.imageHelper.openModal(
            this.selectedTarget.target,
            this.selectedTarget?.type === 'slide',
            false,
            'app-storage-data',
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
    if (this.selectedTarget?.type === 'element' || !this.selectedTarget?.slide?.aspectRatio) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: component,
      componentProps: {
        selectedTarget: this.selectedTarget.target
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu'
    });

    popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        await this.shapeHelper.appendShape(
          this.selectedTarget.target,
          component === 'app-image-element'
            ? {
                img: detail.data
              }
            : detail.data
        );
      }
    });

    await popover.present();
  }

  private async appendText() {
    await this.shapeHelper.appendText(this.selectedTarget.target);
  }

  private async getImagePopover(): Promise<HTMLIonPopoverElement> {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-image-element',
      componentProps: {
        selectedTarget: this.selectedTarget.target,
        slide: this.selectedTarget?.type === 'slide'
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu'
    });

    return popover;
  }

  private async openEditPollSlide() {
    if (this.selectedTarget?.type === 'element' || this.selectedTarget?.slide?.poll) {
      return;
    }

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-poll-options',
      componentProps: {
        selectedTarget: this.selectedTarget,
        slideDidChange: this.slideDidChange
      }
    });

    modal.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async openCode() {
    if (!this.selectedTarget?.element?.code) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-code',
      componentProps: {
        selectedTarget: this.selectedTarget.target,
        codeDidChange: this.codeDidChange
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu'
    });

    await popover.present();
  }

  private async openMath() {
    if (!this.selectedTarget?.element?.math) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-math',
      componentProps: {
        selectedTarget: this.selectedTarget.target,
        mathDidChange: this.mathDidChange
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu'
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
        selectedTarget: this.selectedTarget.target
      }
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data && this.selectedTarget) {
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
        selectedTarget: this.selectedTarget.target
      }
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data && this.selectedTarget) {
        this.notesDidChange.emit(this.selectedTarget.target);
      }

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async transformSlotType(type: SlotType) {
    if (!this.selectedTarget || !this.selectedTarget.target.parentElement) {
      return;
    }

    const element: HTMLElement = await ToggleSlotUtils.toggleSlotType(this.selectedTarget.target, type);

    await this.replaceSlot(element);
  }

  private async transformTemplate(template: InitTemplate) {
    if (!this.selectedTarget || !this.selectedTarget.slide?.fixed) {
      return;
    }

    const slide: JSX.IntrinsicElements = await CloneSlideUtils.toggleTemplate(this.selectedTarget.target, template);

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
    if (!this.selectedTarget || !this.selectedTarget.target.parentElement || !element) {
      return;
    }

    this.selectedTarget.target.parentElement.replaceChild(element, this.selectedTarget.target);

    await this.initSelectedElement(element);

    await this.emitChange();

    await this.resizeSlideContent();

    await this.reset();
  }

  private emitChange(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedTarget || !this.selectedTarget.target.parentElement) {
        resolve();
        return;
      }

      // If not slide, then parent is the container slide
      this.slideDidChange.emit(
        this.selectedTarget?.type === 'slide' ? this.selectedTarget.target : this.selectedTarget.target.parentElement
      );

      resolve();
    });
  }

  private async initSelectedElement(element: HTMLElement | undefined) {
    // If needed, remove highlight on previous element
    if (!element && this.selectedTarget) {
      await this.highlightElement(false);
    }

    this.selectedTarget = element
      ? {
          target: element,
          ...(SelectedElementUtils.initDescription(element) as SelectedTarget)
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
      if (!this.selectedTarget || this.selectedTarget?.type === 'slide') {
        resolve();
        return;
      }

      if (highlight) {
        this.selectedTarget.target.setAttribute('highlighted', '');
      } else {
        this.selectedTarget.target.removeAttribute('highlighted');
      }

      resolve();
    });
  }

  private async attachResizeSlideContent() {
    if (!this.selectedTarget) {
      return;
    }

    if (window && 'ResizeObserver' in window) {
      await this.detachResizeSlideContent();

      this.elementResizeObserver = new ResizeObserver(async (entries) => {
        if (
          entries &&
          entries.length > 0 &&
          entries[0].target &&
          entries[0].target.nodeName &&
          !isSlide(entries[0].target as HTMLElement)
        ) {
          await this.resizeSlideContent();
        }
      });

      this.elementResizeObserver.observe(this.selectedTarget.target);
    } else {
      // Fallback, better  than nothing. It won't place the toolbar if the size on enter or delete  but at least if the content change like if list is toggled
      this.selectedTarget.target.addEventListener('focusout', () => this.debounceResizeSlideContent(), {passive: true});
    }
  }

  private detachResizeSlideContent(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (window && 'ResizeObserver' in window) {
        if (this.elementResizeObserver && this.selectedTarget) {
          this.elementResizeObserver.unobserve(this.selectedTarget.target);
          this.elementResizeObserver.disconnect();
        }
      } else {
        this.selectedTarget.target.removeEventListener('focusout', () => this.debounceResizeSlideContent(), true);
      }

      resolve();
    });
  }

  private async openStyle() {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-element-style',
      componentProps: {
        selectedTarget: this.selectedTarget,
        imgDidChange: this.imgDidChange,
        imageHelper: this.imageHelper
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: `popover-menu ${this.selectedTarget?.slide?.poll ? 'popover-menu-wide' : ''}`
    });

    await popover.present();
  }

  private async openImage() {
    const popover: HTMLIonPopoverElement = await this.getImagePopover();

    popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data) {
        await this.imageHelper.imageAction(this.selectedTarget.target, this.selectedTarget?.type === 'slide', false, detail.data);
      }
    });

    await popover.present();
  }

  private async deleteLogo() {
    await this.imageHelper.deleteSlideAttributeImgSrc(this.selectedTarget.target);
  }

  private updateYoutube = (youtubeUrl: string): Promise<void> => {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget || !this.selectedTarget?.slide?.youtube) {
        resolve();
        return;
      }

      if (!youtubeUrl || youtubeUrl === undefined || youtubeUrl === '') {
        resolve();
        return;
      }

      this.selectedTarget.target.setAttribute('src', youtubeUrl);
      this.slideDidChange.emit(this.selectedTarget.target);

      resolve();
    });
  };

  private updatePlayground = async (playground: PlaygroundAction) => {
    if (!this.selectedTarget || !this.selectedTarget?.slide?.playground) {
      return;
    }

    if (!playground) {
      return;
    }

    if (!playground.src || playground.src === undefined || playground.src === '') {
      return;
    }

    this.selectedTarget.target.setAttribute('src', playground.src);

    if (playground.theme) {
      this.selectedTarget.target.setAttribute('theme', playground.theme);
    } else {
      this.selectedTarget.target.removeAttribute('theme');
    }

    this.slideDidChange.emit(this.selectedTarget.target);
  };

  private updateSlideDemo = async (demoAttr: DemoAction): Promise<void> => {
    if (!this.selectedTarget || !this.selectedTarget?.slide?.demo) {
      return;
    }

    if (!demoAttr || !demoAttr.src || demoAttr.src === undefined || demoAttr.src === '') {
      return;
    }

    const demo: HTMLElement = this.selectedTarget.target.querySelector('deckgo-demo');

    if (!demo) {
      return;
    }

    demo.setAttribute('src', demoAttr.src);
    demo.setAttribute('mode', demoAttr.mode);

    this.slideDidChange.emit(this.selectedTarget.target);
  };

  private toggleList(destinationListType: SlotType.OL | SlotType.UL): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget) {
        resolve();
        return;
      }

      if (SlotUtils.isNodeRevealList(this.selectedTarget.target)) {
        await this.updateRevealListAttribute(destinationListType);
      } else {
        await this.transformSlotType(destinationListType);
      }

      resolve();
    });
  }

  private updateRevealListAttribute(type: SlotType): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget) {
        resolve();
        return;
      }

      this.selectedTarget.target.setAttribute('list-tag', type);

      await this.emitChange();

      await this.reset();

      resolve();
    });
  }

  private resizeSlideContent(slideElement?: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget) {
        resolve();
        return;
      }

      const element: HTMLElement = slideElement
        ? slideElement
        : this.selectedTarget?.type === 'slide'
        ? this.selectedTarget.target
        : this.selectedTarget.target.parentElement;

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
      this.selectedTarget?.slide?.qrCode ||
      this.selectedTarget?.slide?.chart ||
      this.selectedTarget?.slide?.poll ||
      this.selectedTarget?.slide?.youtube ||
      this.selectedTarget?.slide?.playground ||
      this.selectedTarget?.slide?.author ||
      this.selectedTarget?.slide?.demo ||
      this.selectedTarget?.slide?.scope !== SlideScope.DEFAULT
    );
  }

  private async openMoreActions($event: UIEvent) {
    if (!$event) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-more-element-actions',
      componentProps: {
        notes: this.selectedTarget?.type === 'slide',
        clone: this.selectedTarget?.type === 'slide' || this.selectedTarget?.element?.shape !== undefined,
        images: this.selectedTarget?.slide?.aspectRatio,
        transform: this.displayTransform()
      },
      event: $event,
      mode: 'ios'
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
    return (
      (this.selectedTarget?.type === 'element' || this.selectedTarget?.slide?.fixed) && this.selectedTarget?.element?.shape === undefined
    );
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
        disabled={busyStore.state.busy && this.selectedTarget?.type === 'slide'}
        class="wider-devices ion-activatable"
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="trash-bin" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.delete}</ion-label>
      </button>
    );
  }

  private renderNotes() {
    const classElement: string | undefined = `wider-devices ion-activatable ${this.selectedTarget?.type === 'slide' ? '' : 'hidden'}`;

    return (
      <button
        onClick={() => this.openNotes()}
        aria-label={i18n.state.editor.notes}
        disabled={busyStore.state.busy}
        class={classElement}
        tabindex={this.selectedTarget?.type === 'slide' ? 0 : -1}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="create" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.notes}</ion-label>
      </button>
    );
  }

  private renderClone() {
    const displayed: boolean = this.selectedTarget?.type === 'slide' || this.selectedTarget?.element?.shape !== undefined;
    const classSlide: string | undefined = `wider-devices ion-activatable ${displayed ? '' : 'hidden'}`;

    return (
      <button
        onClick={() => this.clone()}
        aria-label={i18n.state.editor.copy}
        disabled={busyStore.state.busy}
        class={classSlide}
        tabindex={displayed ? 0 : -1}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="copy" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.copy}</ion-label>
      </button>
    );
  }

  private renderCopyStyle() {
    const displayed: boolean = this.selectedTarget?.type === 'element' && this.selectedTarget?.element?.shape === undefined;
    const classSlide: string | undefined = `ion-activatable ${displayed ? '' : 'hidden'}`;

    return (
      <button
        onClick={($event: UIEvent) => this.openCopyStyle($event)}
        aria-label={i18n.state.editor.format}
        disabled={busyStore.state.busy}
        class={classSlide}
        tabindex={displayed ? 0 : -1}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="color-wand" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.format}</ion-label>
      </button>
    );
  }

  private renderStyle() {
    return (
      <button onClick={() => this.openStyle()} aria-label={i18n.state.editor.style} class="ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="brush" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.style}</ion-label>
      </button>
    );
  }

  private renderEdit() {
    const displayed: boolean = this.selectedTarget?.type === 'slide' && this.isSlideEditable();
    const classSlide: string | undefined = `ion-activatable${displayed ? '' : ' hidden'}`;

    return (
      <button
        onClick={() =>
          this.selectedTarget?.slide?.poll
            ? this.openEditPollSlide()
            : this.selectedTarget?.slide?.youtube
            ? this.openEditModalSlide('app-youtube', this.updateYoutube)
            : this.selectedTarget?.slide?.playground
            ? this.openEditModalSlide('app-playground', this.updatePlayground)
            : this.selectedTarget?.slide?.demo
            ? this.openEditModalSlide('app-demo', this.updateSlideDemo)
            : this.selectedTarget?.slide?.scope !== SlideScope.DEFAULT
            ? this.openEditSlide()
            : this.openEditSlide()
        }
        aria-label={i18n.state.editor.options}
        class={classSlide}
        tabindex={displayed ? 0 : -1}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="settings" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.options}</ion-label>
      </button>
    );
  }

  private renderTransform() {
    const displayed: boolean = this.displayTransform();
    const classToggle: string | undefined = `wider-devices ion-activatable${displayed ? '' : ' hidden'}`;

    return (
      <button
        aria-label={i18n.state.editor.transform}
        onClick={() => this.openTransform()}
        class={classToggle}
        tabindex={displayed ? 0 : -1}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="flask" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.transform}</ion-label>
      </button>
    );
  }

  private renderAspectRatio() {
    const displayed: boolean = this.selectedTarget?.slide?.aspectRatio;
    const classSlide: string | undefined = `ion-activatable${displayed ? '' : ' hidden'}`;

    return [
      <button onClick={() => this.appendText()} aria-label={i18n.state.editor.add_text} class={classSlide} tabindex={displayed ? 0 : -1}>
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="text" path="icons" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.add_text}</ion-label>
      </button>,
      <button
        onClick={() => this.openShape('app-shape')}
        aria-label={i18n.state.editor.add_shape}
        class={classSlide}
        tabindex={displayed ? 0 : -1}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="shapes" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.add_shape}</ion-label>
      </button>,
      <button
        onClick={() => this.openShape('app-image-element')}
        aria-label={i18n.state.editor.add_image}
        class={`wider-devices ${classSlide}`}
        tabindex={displayed ? 0 : -1}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="images" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.add_image}</ion-label>
      </button>
    ];
  }

  private renderCodeOptions() {
    const classSlideCode: string | undefined = `ion-activatable${this.selectedTarget?.element?.code ? '' : ' hidden'}`;

    return (
      <button
        onClick={() => this.openCode()}
        aria-label={i18n.state.editor.options}
        class={classSlideCode}
        tabindex={this.selectedTarget?.element?.code ? 0 : -1}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="settings" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.options}</ion-label>
      </button>
    );
  }
  private renderMathOptions() {
    const classSlideMath: string | undefined = `ion-activatable${this.selectedTarget?.element?.math ? '' : ' hidden'}`;

    return (
      <button
        onClick={() => this.openMath()}
        aria-label={i18n.state.editor.options}
        class={classSlideMath}
        tabindex={this.selectedTarget?.element?.math ? 0 : -1}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="settings" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.options}</ion-label>
      </button>
    );
  }

  private renderImages() {
    const classImage: string | undefined = `ion-activatable${this.selectedTarget?.element?.image ? '' : ' hidden'}`;

    return (
      <button
        onClick={() => this.openImage()}
        aria-label={i18n.state.editor.image}
        class={classImage}
        tabindex={this.selectedTarget?.element?.image ? 0 : -1}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="images" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.image}</ion-label>
      </button>
    );
  }

  private renderMore() {
    return (
      <button onClick={(e: UIEvent) => this.openMoreActions(e)} disabled={busyStore.state.busy} class="small-devices ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="ellipsis-vertical" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.more}</ion-label>
      </button>
    );
  }
}
