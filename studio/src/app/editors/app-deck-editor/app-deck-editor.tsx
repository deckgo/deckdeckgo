import {isSlide} from '@deckdeckgo/deck-utils';
import {convertStyle, elementIndex, SlideTemplate, throwError} from '@deckdeckgo/editor';
import {getEdit} from '@deckdeckgo/offline';
import {SlotType} from '@deckdeckgo/studio';
import {ChartEvents, ImageLoadEvents} from '@deckdeckgo/sync';
import {debounce, isAndroidTablet, isFullscreen, isIOS, isIPad, isMobile} from '@deckdeckgo/utils';
import type {ItemReorderEventDetail, OverlayEventDetail} from '@ionic/core';
import {modalController, popoverController} from '@ionic/core';
import {StyloConfigToolbar, StyloPaletteColor} from '@papyrs/stylo';
import {Component, ComponentInterface, Element, h, JSX, Listen, Method, State} from '@stencil/core';
import {get, set} from 'idb-keyval';
import {EnvironmentGoogleConfig} from '../../config/environment-config';
import {CodeEvents} from '../../events/editor/code/code.events';
import {DeckDataEvents} from '../../events/editor/deck/deck.data.events';
import {DeckEditorEvents} from '../../events/editor/editor/deck.editor.events';
import {PollEvents} from '../../events/editor/poll/poll.events';
import {RemoteEvents} from '../../events/editor/remote/remote.events';
import {SlideHelper} from '../../helpers/editor/slide.helper';
import {FontsService} from '../../services/editor/fonts/fonts.service';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import authStore from '../../stores/auth.store';
import busyStore from '../../stores/busy.store';
import colorStore from '../../stores/color.store';
import editorStore from '../../stores/editor.store';
import i18n from '../../stores/i18n.store';
import undoRedoStore from '../../stores/undo-redo.store';
import {Editor} from '../../types/editor/editor';
import {cloud} from '../../utils/core/environment.utils';
import {signIn as navigateSignIn} from '../../utils/core/signin.utils';
import {ColorUtils} from '../../utils/editor/color.utils';
import {CreateSlidesUtils} from '../../utils/editor/create-slides.utils';
import {ParseDeckSlotsUtils} from '../../utils/editor/parse-deck-slots.utils';

@Component({
  tag: 'app-deck-editor',
  styleUrl: 'app-deck-editor.scss'
})
export class AppDeckEditor implements ComponentInterface {
  @Element() el: HTMLElement;

  @State()
  private slides: JSX.IntrinsicElements[] = [];

  @State()
  private background: JSX.IntrinsicElements | undefined;

  @State()
  private header: JSX.IntrinsicElements | undefined;

  @State()
  private footer: JSX.IntrinsicElements | undefined;

  @State()
  private style: Record<string, string> | undefined;

  @State()
  private animation: 'slide' | 'fade' | 'none' = 'slide';

  @State()
  private direction: 'horizontal' | 'vertical' | 'papyrus' = 'horizontal';

  @State()
  private directionMobile: 'horizontal' | 'vertical' | 'papyrus' = 'papyrus';

  @State()
  private presenting: boolean = false;

  @State()
  private activeIndex: number = 0;

  private readonly deckDataEvents: DeckDataEvents = new DeckDataEvents();
  private readonly deckEditorEvents: DeckEditorEvents = new DeckEditorEvents();
  private readonly remoteEvents: RemoteEvents = new RemoteEvents();
  private readonly pollEvents: PollEvents = new PollEvents();
  private readonly imageEvents: ImageLoadEvents = new ImageLoadEvents();
  private readonly chartEvents: ChartEvents = new ChartEvents();
  private readonly codeEvents: CodeEvents = new CodeEvents();

  private editorHelper: SlideHelper = new SlideHelper();

  private fontsService: FontsService;

  @State()
  private slidesFetched: boolean = false;

  @State()
  private fullscreen: boolean = false;

  @State()
  private thumbnails: boolean = false;

  @State()
  private mainSize: {width: string; height: string};

  private mobile: boolean = isMobile();

  private destroyBusyListener;
  private destroyAuthListener;

  private deckRef!: HTMLDeckgoDeckElement;
  private actionsEditorRef!: HTMLAppActionsDeckEditorElement;
  private contentRef!: HTMLElement;
  private mainRef!: HTMLElement;
  private breadCrumbsRef!: HTMLAppBreadcrumbsElement;

  private mainResizeObserver: ResizeObserver;
  private slideResizeObserver: ResizeObserver;

  @State()
  private editorConfig: Partial<StyloConfigToolbar> = {
    style: {
      list: false,
      align: false,
      fontSize: false,
      backgroundColor: true
    }
  };

  constructor() {
    this.fontsService = FontsService.getInstance();
  }

  componentWillLoad() {
    this.imageEvents.init();
    this.chartEvents.init();
    this.codeEvents.init();

    this.updateEditorToolbarConfig();
  }

  async componentDidLoad() {
    await this.init();

    await this.initResize();

    await this.updateInlineEditorListener();

    await this.remoteEvents.init(this.el);
    await this.pollEvents.init(this.el);

    this.initWindowResize();
  }

  async disconnectedCallback() {
    await this.destroy();
    await this.disconnect();
  }

  async init() {
    await this.deckDataEvents.init(this.mainRef);
    await this.deckEditorEvents.init({mainRef: this.mainRef, actionsEditorRef: this.actionsEditorRef});

    await this.initOrFetch();

    this.fullscreen = isFullscreen() && !this.mobile;
  }

  @Listen('colorChange', {target: 'document', passive: true})
  onColorChange({detail}: CustomEvent<StyloPaletteColor>) {
    ColorUtils.updateColor(detail);

    this.updateEditorToolbarConfig();
  }

  private updateEditorToolbarConfig() {
    this.editorConfig = {
      ...this.editorConfig,
      palette: colorStore.state.history.slice(0, 11)
    };
  }

  @Method()
  async initNewDeck() {
    this.slidesFetched = false;

    await this.reset();

    await this.initOrFetch();

    await this.deckRef.slideTo(0);

    // Select deck
    this.actionsEditorRef?.selectStep(undefined);
  }

  private async initOrFetch() {
    const editor: Editor | undefined = await getEdit();
    const deckId: string | undefined = editor?.id;

    if (!deckId) {
      await this.initSlide();
    } else {
      await this.fetchSlides(deckId);
    }

    this.slidesFetched = true;
  }

  async destroy() {
    this.deckDataEvents.destroy();
    this.deckEditorEvents.destroy();
    this.pollEvents.destroy();
    this.imageEvents.destroy();
    this.chartEvents.destroy();
    this.codeEvents.destroy();

    await this.remoteEvents.destroy();

    editorStore.reset();
    undoRedoStore.reset();

    await this.remoteEvents.destroy();
  }

  private async reset() {
    this.background = undefined;
    this.header = undefined;
    this.footer = undefined;
    this.style = undefined;
    this.animation = 'slide';
    this.direction = 'horizontal';
    this.directionMobile = 'papyrus';

    this.resetDOM();

    this.activeIndex = 0;

    this.slides = [];

    editorStore.reset();
    undoRedoStore.reset();
  }

  // TODO: It would be cleaner to have the states in store and to keep these in sync instead of modifying the DOM
  private resetDOM() {
    const background: HTMLElement = this.deckRef?.querySelector(":scope > [slot='background']");
    background?.parentElement?.removeChild(background);

    const header: HTMLElement = this.deckRef?.querySelector(":scope > [slot='header']");
    header?.parentElement?.removeChild(header);

    const footer: HTMLElement = this.deckRef?.querySelector(":scope > [slot='footer']");
    footer?.parentElement?.removeChild(footer);

    this.deckRef?.setAttribute('style', '');
    this.deckRef?.setAttribute('animation', this.animation);
    this.deckRef?.setAttribute('direction', this.direction);
    this.deckRef?.setAttribute('direction-mobile', this.directionMobile);
  }

  private async disconnect() {
    this.removeWindowResize();

    if (this.destroyBusyListener) {
      this.destroyBusyListener();
    }

    if (this.destroyAuthListener) {
      this.destroyAuthListener();
    }

    if (this.slideResizeObserver) {
      this.slideResizeObserver.unobserve(this.mainRef);
      this.slideResizeObserver.disconnect();
    }

    if (this.mainResizeObserver) {
      this.mainResizeObserver.unobserve(this.contentRef);
      this.mainResizeObserver.unobserve(this.actionsEditorRef);
      this.mainResizeObserver.disconnect();
    }
  }

  private async updateInlineEditorListener(): Promise<void> {
    if (!this.deckRef) {
      return;
    }

    const inlineEditor: HTMLStyloToolbarElement = this.el.querySelector('stylo-toolbar');

    if (!inlineEditor) {
      return;
    }

    inlineEditor.containerRef = this.deckRef;
  }

  private async initSlide() {
    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide({
      template: SlideTemplate.TITLE,
      elements: [SlotType.H1, SlotType.SECTION]
    });

    await this.concatSlide(slide);
  }

  private async fetchSlides(deckId: string) {
    const slides: JSX.IntrinsicElements[] = await this.editorHelper.loadDeckAndRetrieveSlides(deckId);

    if (slides && slides.length > 0) {
      this.slides = [...slides];
    }

    await this.initDeckStyle();
  }

  private async initDeckStyle() {
    if (editorStore.state.deck?.data?.attributes?.style) {
      this.style = convertStyle(editorStore.state.deck.data.attributes.style);
    } else {
      this.style = undefined;
    }

    if (editorStore.state.deck?.data?.attributes?.animation) {
      this.animation = editorStore.state.deck.data.attributes.animation;
    }

    if (editorStore.state.deck?.data?.attributes?.direction) {
      this.direction = editorStore.state.deck.data.attributes.direction;
    }

    if (editorStore.state.deck?.data?.attributes?.directionMobile) {
      this.directionMobile = editorStore.state.deck.data.attributes.directionMobile;
    }

    this.background = await ParseDeckSlotsUtils.convert(editorStore.state.deck.data.background, 'background');
    this.header = await ParseDeckSlotsUtils.convert(editorStore.state.deck.data.header, 'header');
    this.footer = await ParseDeckSlotsUtils.convert(editorStore.state.deck.data.footer, 'footer');

    const google: EnvironmentGoogleConfig = EnvironmentConfigService.getInstance().get('google');
    await this.fontsService.loadGoogleFont(google.fontsUrl, this.style);
  }

  private async concatSlide(extraSlide: JSX.IntrinsicElements) {
    const slideIndex: number = this.activeIndex + 1;
    this.slides = [...this.slides.slice(0, slideIndex), extraSlide, ...this.slides.slice(slideIndex)];

    await ParseDeckSlotsUtils.stickLastChildren(this.mainRef);

    await this.deckRef?.slideTo(slideIndex);
  }

  private async replaceSlide(slide: JSX.IntrinsicElements) {
    this.slides = [...this.slides.map((filteredSlide: JSX.IntrinsicElements, i) => (i === this.activeIndex ? slide : filteredSlide))];
  }

  private async animatePrevNextSlide($event: CustomEvent<boolean>) {
    if (!$event) {
      return;
    }

    if (!this.deckRef) {
      return;
    }

    if ($event.detail) {
      await this.deckRef.slideNext(false, true);
    } else {
      await this.deckRef.slidePrev(false, true);
    }
  }

  private async copySlide($event: CustomEvent<HTMLElement>) {
    if (!$event || !$event.detail) {
      return;
    }

    const slide: JSX.IntrinsicElements = await this.editorHelper.copySlide($event.detail);

    if (slide) {
      await this.concatSlide(slide);
    }
  }

  private async transformSlide($event: CustomEvent<JSX.IntrinsicElements>) {
    if (!$event || !$event.detail) {
      return;
    }

    await this.replaceSlide($event.detail);
  }

  @Listen('slideDelete', {target: 'document'})
  async deleteSlide({detail: deletedSlide}: CustomEvent<HTMLElement>) {
    const slideIndex: number = elementIndex(deletedSlide);

    this.slides = [...this.slides.filter((_slide: JSX.IntrinsicElements, index: number) => slideIndex !== index)];

    // Update deck length and slide to an active slide
    await this.deckRef.deleteActiveSlide(false);
  }

  @Listen('addSlide', {target: 'document'})
  async addSlide($event: CustomEvent<JSX.IntrinsicElements>) {
    if (!$event) {
      return;
    }

    await this.concatSlide($event.detail);
  }

  @Listen('openEmbed', {target: 'document'})
  async openEmbed() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-embed'
    });

    await modal.present();
  }

  @Listen('actionPublish', {target: 'document'})
  async onActionPublish() {
    // No slides, no publish
    if (!this.slides || this.slides.length <= 0) {
      return;
    }

    if (!cloud()) {
      throwError('No cloud provider to publish material.');
      return;
    }

    if (!authStore.state.authUser) {
      await this.signIn();
      return;
    }

    await this.deckEditorEvents.blockSlide(true);

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-publish',
      cssClass: 'fullscreen'
    });

    modal.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
      await this.deckEditorEvents.blockSlide(false);
    });

    await modal.present();
  }

  /**
   * When an element is focused, we check if its related slide index is the current one aka is the element on a previous or next slide.
   * If these index doesn't match, we move the deck to the new slide.
   * This could happens in case the user tap "Tab" to switch between elements.
   * @param $event
   */
  private onElementFocus($event: CustomEvent<HTMLElement>): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      const selectedTarget: HTMLElement = $event.detail;

      if (isSlide(selectedTarget)) {
        resolve();
        return;
      }

      const slide: HTMLElement = selectedTarget.parentElement;

      if (!slide || !slide.parentNode) {
        resolve();
        return;
      }

      if (this.activeIndex < 0) {
        resolve();
        return;
      }

      const selectedElementSlideIndex: number = Array.prototype.indexOf.call(slide.parentNode.children, slide);

      if (selectedElementSlideIndex === this.activeIndex) {
        resolve();
        return;
      }

      if (!this.deckRef) {
        resolve();
        return;
      }

      await this.deckRef.slideTo(selectedElementSlideIndex);

      resolve();
    });
  }

  private deckTouched($event: MouseEvent | TouchEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event) {
        resolve();
        return;
      }

      if (!$event.target || !($event.target instanceof HTMLElement)) {
        resolve();
        return;
      }

      if ($event instanceof MouseEvent && this.mobile) {
        resolve();
        return;
      }

      // Click on the pager
      if (!($event.target as HTMLElement).nodeName || ($event.target as HTMLElement).nodeName.toLowerCase() === 'deckgo-deck') {
        resolve();
        return;
      }

      // Need to move a bit the mouse first to detect that we want to edit, otherwise we might select the deck with a click without displaying the toolbar footer
      if (this.fullscreen && this.presenting) {
        resolve();
        return;
      }

      const element: HTMLElement = $event.target as HTMLElement;

      await this.touchToolbar(element);

      resolve();
    });
  }

  private async touchToolbar(element: HTMLElement) {
    if (!this.actionsEditorRef) {
      return;
    }

    await this.actionsEditorRef.touch(element);
  }

  private async selectDeck($event: MouseEvent | TouchEvent) {
    const src: HTMLElement = $event.composedPath()[0] as HTMLElement;

    if (!this.contentRef.isEqualNode(src) && !this.breadCrumbsRef.isEqualNode(src)) {
      return;
    }

    await this.actionsEditorRef.selectDeck();
  }

  @Listen('toggleFullScreen', {target: 'window'})
  async onToggleFullScreen() {
    if (!this.deckRef) {
      return;
    }

    await this.deckEditorEvents.selectDeck();
    await this.deckRef.toggleFullScreen();

    await this.openFullscreenInfo();
  }

  private async openFullscreenInfo() {
    const infoDisplayedOnce: boolean = await get<boolean>('deckdeckgo_display_fullscreen_info');

    if (infoDisplayedOnce) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-fullscreen-info',
      mode: 'ios',
      cssClass: 'info',
      showBackdrop: true
    });

    popover.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
      await set('deckdeckgo_display_fullscreen_info', true);
    });

    await popover.present();
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onWindowResize));
    }
  }

  private removeWindowResize() {
    if (window) {
      window.removeEventListener('resize', debounce(this.onWindowResize));
    }
  }

  private onWindowResize = async () => {
    this.fullscreen = isFullscreen() && !isIOS();

    // Per default, when we switch to the fullscreen mode, we want to present the presentation not edit it
    await this.updatePresenting(this.fullscreen);

    await this.initSizeOldBrowser();
  };

  private async initResize() {
    if (window && 'ResizeObserver' in window) {
      this.initMainSizeObserver();
      this.initSlideSizeObserver();
    } else {
      await this.initSizeOldBrowser();
    }
  }

  private async initSizeOldBrowser() {
    if (window && 'ResizeObserver' in window) {
      return;
    }

    setTimeout(async () => {
      this.initMainSize();
      await this.initSlideSize();
      this.initThumbnails();
    }, 100);
  }

  private initMainSizeObserver() {
    this.mainResizeObserver = new ResizeObserver((_entries) => {
      this.initMainSize();
      this.initThumbnails();
    });

    if (this.contentRef) {
      this.mainResizeObserver.observe(this.contentRef);
    }

    if (this.actionsEditorRef) {
      this.mainResizeObserver.observe(this.actionsEditorRef);
    }
  }

  private initThumbnails() {
    const wideScreen: MediaQueryList = window.matchMedia('(min-width: 1201px)');

    this.thumbnails = !isFullscreen() && wideScreen.matches;
  }

  private initMainSize() {
    if (!this.contentRef || isFullscreen() || (this.mobile && !isIPad() && !isAndroidTablet())) {
      this.mainSize = {
        width: this.mobile ? 'calc(100% - 32px)' : '100%',
        height: this.mobile ? 'calc(100% - 32px)' : '100%'
      };
      return;
    }

    const maxHeight: number = this.contentRef.offsetHeight - 32;

    const wideScreen: MediaQueryList = window.matchMedia('(min-width: 1200px)');

    const width: number = this.contentRef.offsetWidth - (wideScreen.matches ? 164 : 32);
    const height: number = (width * 9) / 16;

    this.mainSize =
      height > maxHeight
        ? {
            width: `${(maxHeight * 16) / 9}px`,
            height: `${maxHeight}px`
          }
        : {
            width: `${width}px`,
            height: `${height}px`
          };
  }

  private initSlideSizeObserver() {
    if (!this.mainRef) {
      return;
    }

    this.slideResizeObserver = new ResizeObserver(async (_entries) => {
      await this.initSlideSize();

      if (this.activeIndex < 0) {
        return;
      }

      if (typeof this.deckRef?.slideTo === 'function') {
        await this.deckRef?.slideTo(this.activeIndex);
      }
    });

    this.slideResizeObserver.observe(this.mainRef);
  }

  private async initSlideSize() {
    await this.deckDataEvents.initSlideSize();
  }

  @Listen('signIn', {target: 'document'})
  signIn() {
    navigateSignIn();
  }

  @Listen('reorder', {target: 'document'})
  async onReorderSlides($event: CustomEvent<ItemReorderEventDetail>) {
    if (!$event || !$event.detail) {
      return;
    }

    await this.reorderSlides($event.detail);
  }

  private async reorderSlides(detail: ItemReorderEventDetail) {
    if (!detail) {
      return;
    }

    if (detail.from < 0 || detail.to < 0 || !this.slides || detail.to >= this.slides.length || detail.from === detail.to) {
      return;
    }

    try {
      await this.deckDataEvents.updateDeckSlidesOrder(detail);

      await this.remoteEvents.updateRemoteSlidesOnMutation();

      this.slides.splice(detail.to, 0, ...this.slides.splice(detail.from, 1));
      this.slides = [...this.slides];
    } catch (err) {
      // We ignore the error here
    }

    // Finish the reorder and position the item in the DOM based on where the gesture ended. This method can also be called directly by the reorder group
    detail.complete();
  }

  private async updatePresenting(presenting: boolean) {
    this.presenting = presenting;

    await this.remoteEvents.updateRemoteReveal(this.fullscreen && this.presenting);

    await this.deckDataEvents.toggleSlideEditable(!this.presenting);
  }

  @Listen('remoteSlideDidChange', {target: 'document'})
  async onRemoteSlideDidChange() {
    await this.onSlideChange();
  }

  private async onSlideChange() {
    await this.deckDataEvents.toggleSlideEditable(!this.fullscreen || !this.presenting);

    const index: number = await this.deckRef?.getActiveIndex();

    if (index < 0 || this.activeIndex === index) {
      return;
    }

    this.activeIndex = index;
  }

  private async selectStep($event: CustomEvent<HTMLElement>) {
    this.actionsEditorRef?.selectStep($event?.detail);
  }

  @Listen('deckDidLoad', {target: 'document'})
  onDeckDidLoad() {
    busyStore.state.deckReady = true;
  }

  render() {
    const autoSlide: boolean =
      editorStore.state.deck?.data?.attributes?.autoSlide !== undefined ? editorStore.state.deck.data.attributes.autoSlide : false;

    return [
      <ion-content
        class={`ion-no-padding ${busyStore.state.deckReady ? 'ready' : ''}`}
        onClick={($event: MouseEvent | TouchEvent) => this.selectDeck($event)}>
        <div class="editor">
          {this.renderSlidesThumbnails()}

          <div class="grid">
            <div class="deck" ref={(el) => (this.contentRef = el as HTMLElement)}>
              <main
                ref={(el) => (this.mainRef = el as HTMLElement)}
                class={busyStore.state.slideReady ? (this.presenting ? 'ready idle' : 'ready') : undefined}
                style={{'--main-size-width': this.mainSize?.width, '--main-size-height': this.mainSize?.height}}>
                {this.renderLoading()}
                <deckgo-deck
                  ref={(el) => (this.deckRef = el as HTMLDeckgoDeckElement)}
                  embedded={true}
                  style={this.style}
                  reveal={this.fullscreen && this.presenting}
                  direction={this.direction}
                  directionMobile={this.directionMobile}
                  animation={this.animation}
                  autoSlide={this.fullscreen && this.presenting && autoSlide ? 'true' : 'false'}
                  onMouseDown={(e: MouseEvent) => this.deckTouched(e)}
                  onTouchStart={(e: TouchEvent) => this.deckTouched(e)}
                  onSlideNextDidChange={() => this.onSlideChange()}
                  onSlidePrevDidChange={() => this.onSlideChange()}
                  onSlideToChange={() => this.onSlideChange()}>
                  {this.slides}
                  {this.background}
                  {this.header}
                  {this.footer}
                </deckgo-deck>
                <deckgo-remote autoConnect={false}></deckgo-remote>
                <app-slide-warning></app-slide-warning>
                {this.renderInlineEditor()}
              </main>
            </div>

            <app-breadcrumbs
              ref={(el) => (this.breadCrumbsRef = el as HTMLAppBreadcrumbsElement)}
              slideNumber={this.activeIndex}
              onStepTo={($event: CustomEvent<HTMLElement>) => this.selectStep($event)}></app-breadcrumbs>

            <app-actions-deck-editor
              ref={(el) => (this.actionsEditorRef = el as HTMLAppActionsDeckEditorElement)}
              fullscreen={this.fullscreen}
              slides={this.slides}
              slideNumber={this.activeIndex}
              onAnimatePrevNextSlide={($event: CustomEvent<boolean>) => this.animatePrevNextSlide($event)}
              onSlideCopy={($event: CustomEvent<HTMLElement>) => this.copySlide($event)}
              onSlideTransform={($event: CustomEvent<JSX.IntrinsicElements>) => this.transformSlide($event)}
              onElementFocus={($event: CustomEvent<HTMLElement>) => this.onElementFocus($event)}
              onPresenting={($event: CustomEvent<boolean>) => this.updatePresenting($event?.detail)}></app-actions-deck-editor>
          </div>
        </div>
        {this.renderSlidePreview()}
        {this.renderLaserPointer()}
      </ion-content>
    ];
  }

  private renderLaserPointer() {
    return this.fullscreen && this.presenting ? <deckgo-laser-pointer></deckgo-laser-pointer> : undefined;
  }

  private renderInlineEditor() {
    if (this.mobile) {
      return undefined;
    }

    return <stylo-toolbar config={this.editorConfig}></stylo-toolbar>;
  }

  private renderLoading() {
    if (this.slidesFetched && busyStore.state.deckReady) {
      return undefined;
    } else {
      return (
        <app-spinner>
          <p>{i18n.state.editor.loading}</p>
        </app-spinner>
      );
    }
  }

  private renderSlidesThumbnails() {
    if (!this.thumbnails) {
      return undefined;
    }

    return (
      <app-slides-aside
        deckRef={this.deckRef}
        activeIndex={this.activeIndex}
        onStepTo={($event: CustomEvent<HTMLElement>) => this.selectStep($event)}></app-slides-aside>
    );
  }

  private renderSlidePreview() {
    if (this.thumbnails) {
      return undefined;
    }

    return <app-slide-preview deckRef={this.deckRef}></app-slide-preview>;
  }
}
