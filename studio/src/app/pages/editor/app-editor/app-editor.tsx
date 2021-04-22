import {Build, Component, Element, h, JSX, Listen, Prop, State} from '@stencil/core';

import {ItemReorderEventDetail, modalController, OverlayEventDetail, popoverController} from '@ionic/core';

import {get, set} from 'idb-keyval';

import deckStore from '../../../stores/deck.store';
import busyStore from '../../../stores/busy.store';
import authStore from '../../../stores/auth.store';
import colorStore from '../../../stores/color.store';

import {debounce, isAndroidTablet, isFullscreen, isIOS, isIPad, isMobile} from '@deckdeckgo/utils';

import {convertStyle, isSlide} from '@deckdeckgo/deck-utils';

import {AuthUser} from '../../../models/auth/auth.user';
import {SlideTemplate} from '../../../models/data/slide';

import {CreateSlidesUtils} from '../../../utils/editor/create-slides.utils';
import {ParseDeckSlotsUtils} from '../../../utils/editor/parse-deck-slots.utils';

import {DeckEventsHandler} from '../../../handlers/editor/events/deck/deck-events.handler';
import {RemoteEventsHandler} from '../../../handlers/editor/events/remote/remote-events.handler';
import {EditorEventsHandler} from '../../../handlers/editor/events/editor/editor-events.handler';
import {PollEventsHandler} from '../../../handlers/editor/events/poll/poll-events.handler';
import {ImageEventsHandler} from '../../../handlers/core/events/image/image-events.handler';
import {ChartEventsHandler} from '../../../handlers/core/events/chart/chart-events.handler';

import {EditorHelper} from '../../../helpers/editor/editor.helper';

import {SlotType} from '../../../types/editor/slot-type';
import {signIn as navigateSignIn} from '../../../utils/core/signin.utils';

import {AuthService} from '../../../services/auth/auth.service';
import {AnonymousService} from '../../../services/editor/anonymous/anonymous.service';
import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';
import {OfflineService} from '../../../services/editor/offline/offline.service';
import {FontsService} from '../../../services/editor/fonts/fonts.service';

import {EnvironmentGoogleConfig} from '../../../types/core/environment-config';

@Component({
  tag: 'app-editor',
  styleUrl: 'app-editor.scss',
})
export class AppEditor {
  @Element() el: HTMLElement;

  @Prop({mutable: true})
  deckId: string;

  @State()
  private slides: JSX.IntrinsicElements[] = [];

  @State()
  private background: JSX.IntrinsicElements | undefined;

  @State()
  private header: JSX.IntrinsicElements | undefined;

  @State()
  private footer: JSX.IntrinsicElements | undefined;

  @State()
  private style: any;

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

  private deckEventsHandler: DeckEventsHandler = new DeckEventsHandler();
  private remoteEventsHandler: RemoteEventsHandler = new RemoteEventsHandler();
  private editorEventsHandler: EditorEventsHandler = new EditorEventsHandler();
  private pollEventsHandler: PollEventsHandler = new PollEventsHandler();
  private imageEventsHandler: ImageEventsHandler = new ImageEventsHandler();
  private chartEventsHandler: ChartEventsHandler = new ChartEventsHandler();

  private editorHelper: EditorHelper = new EditorHelper();

  private authService: AuthService;
  private anonymousService: AnonymousService;

  private offlineService: OfflineService;

  private fontsService: FontsService;

  @State()
  private slidesFetched: boolean = false;

  @State()
  private hideActions: boolean = false;

  @State()
  private hideNavigation: boolean = false;

  @State()
  private fullscreen: boolean = false;

  @State()
  private mainSize: {width: string; height: string};

  private destroyBusyListener;
  private destroyAuthListener;

  private deckRef!: HTMLDeckgoDeckElement;
  private actionsEditorRef!: HTMLAppActionsEditorElement;
  private contentRef!: HTMLElement;
  private mainRef!: HTMLElement;

  private mainResizeObserver: ResizeObserverConstructor;
  private slideResizeObserver: ResizeObserverConstructor;

  constructor() {
    this.authService = AuthService.getInstance();
    this.anonymousService = AnonymousService.getInstance();
    this.offlineService = OfflineService.getInstance();
    this.fontsService = FontsService.getInstance();
  }

  @Listen('ionRouteDidChange', {target: 'window'})
  async onRouteDidChange($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    // ionViewDidEnter and ionViewDidLeave, kind of
    if ($event.detail.to && $event.detail.to.indexOf('/editor') === 0) {
      await this.init();
    } else if ($event.detail.from && $event.detail.from.indexOf('/editor') === 0) {
      await this.destroy();
    }
  }

  async init() {
    await this.deckEventsHandler.init(this.mainRef);
    await this.editorEventsHandler.init(this.mainRef);

    await this.initOffline();

    await this.initWithAuth();

    this.fullscreen = isFullscreen() && !isMobile();
  }

  private async initWithAuth() {
    if (!authStore.state.authUser) {
      // As soon as the anonymous is created, we proceed
      this.destroyAuthListener = authStore.onChange('authUser', async (authUser: AuthUser | null) => {
        if (authUser) {
          await this.initOrFetch();
        }

        this.destroyAuthListener();
      });

      // If no user create an anonymous one
      await this.authService.signInAnonymous();
    } else {
      // We have got a user, regardless if anonymous or not, we init
      await this.initOrFetch();
    }
  }

  private async initOrFetch() {
    if (!this.deckId) {
      await this.initSlide();
    } else {
      await this.fetchSlides();
    }

    this.slidesFetched = true;
  }

  async initOffline() {
    if (Build.isServer) {
      return;
    }

    // if we are offline we can't create a new deck or edit another one that the one we have marked as currently being edited offline
    const offline: OfflineDeck = await this.offlineService.status();
    if (offline !== undefined) {
      this.deckId = offline.id;
    }
  }

  async destroy() {
    this.deckEventsHandler.destroy();
    this.editorEventsHandler.destroy();
    this.pollEventsHandler.destroy();
    this.imageEventsHandler.destroy();
    this.chartEventsHandler.destroy();

    await this.remoteEventsHandler.destroy();

    deckStore.reset();
  }

  async componentDidLoad() {
    await this.initResize();

    await this.updateInlineEditorListener();

    await this.remoteEventsHandler.init(this.el);
    await this.pollEventsHandler.init(this.el);
    await this.imageEventsHandler.init();
    await this.chartEventsHandler.init();

    this.initWindowResize();
  }

  async disconnectedCallback() {
    await this.remoteEventsHandler.destroy();

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

  private updateInlineEditorListener(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.deckRef) {
        return;
      }

      const inlineEditor: HTMLElement = this.el.querySelector('deckgo-inline-editor');

      if (!inlineEditor) {
        return;
      }

      (inlineEditor as any).attachTo = this.deckRef;

      resolve();
    });
  }

  private initSlide(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!document) {
        resolve();
        return;
      }

      const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide({
        template: SlideTemplate.TITLE,
        elements: [SlotType.H1, SlotType.SECTION],
      });

      await this.concatSlide(slide);

      resolve();
    });
  }

  private fetchSlides(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!document || !this.deckId) {
        resolve();
        return;
      }

      const slides: JSX.IntrinsicElements[] = await this.editorHelper.loadDeckAndRetrieveSlides(this.deckId);

      if (slides && slides.length > 0) {
        this.slides = [...slides];
      }

      await this.initDeckStyle();

      resolve();
    });
  }

  private async initDeckStyle() {
    if (deckStore.state.deck?.data?.attributes?.style) {
      this.style = await convertStyle(deckStore.state.deck.data.attributes.style);
    } else {
      this.style = undefined;
    }

    if (deckStore.state.deck?.data?.attributes?.animation) {
      this.animation = deckStore.state.deck.data.attributes.animation;
    }

    if (deckStore.state.deck?.data?.attributes?.direction) {
      this.direction = deckStore.state.deck.data.attributes.direction;
    }

    if (deckStore.state.deck?.data?.attributes?.directionMobile) {
      this.directionMobile = deckStore.state.deck.data.attributes.directionMobile;
    }

    this.background = await ParseDeckSlotsUtils.convert(deckStore.state.deck.data.background, 'background');
    this.header = await ParseDeckSlotsUtils.convert(deckStore.state.deck.data.header, 'header');
    this.footer = await ParseDeckSlotsUtils.convert(deckStore.state.deck.data.footer, 'footer');

    const google: EnvironmentGoogleConfig = EnvironmentConfigService.getInstance().get('google');
    await this.fontsService.loadGoogleFont(google.fontsUrl, this.style);
  }

  private async concatSlide(extraSlide: JSX.IntrinsicElements) {
    this.slides = [...this.slides, extraSlide];

    await ParseDeckSlotsUtils.stickLastChildren(this.el);
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

  private async slideTo($event: CustomEvent<number>) {
    if (!$event) {
      return;
    }

    if (!this.deckRef) {
      return;
    }

    await this.deckRef.slideTo($event.detail);
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

  private async addSlide($event: CustomEvent<JSX.IntrinsicElements>) {
    if (!$event) {
      return;
    }

    await this.concatSlide($event.detail);
  }

  @Listen('actionPublish')
  async onActionPublish() {
    // No slides, no publish
    if (!this.slides || this.slides.length <= 0) {
      return;
    }

    const couldPublish: boolean = await this.anonymousService.couldPublish(this.slides);

    if (!couldPublish) {
      await this.signIn();
      return;
    }

    await this.editorEventsHandler.blockSlide(true);

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-publish',
      cssClass: 'fullscreen',
    });

    modal.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
      await this.editorEventsHandler.blockSlide(false);
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

      const selectedElement: HTMLElement = $event.detail;

      if (isSlide(selectedElement)) {
        resolve();
        return;
      }

      const slide: HTMLElement = selectedElement.parentElement;

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

      if ($event instanceof MouseEvent && isMobile()) {
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

  @Listen('toggleFullScreen', {target: 'window'})
  async onToggleFullScreen() {
    if (!this.deckRef) {
      return;
    }

    await this.editorEventsHandler.selectDeck();
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
      showBackdrop: true,
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
    }, 100);
  }

  private initMainSizeObserver() {
    this.mainResizeObserver = new ResizeObserver((_entries) => {
      this.initMainSize();
    });

    if (this.contentRef) {
      this.mainResizeObserver.observe(this.contentRef);
    }

    if (this.actionsEditorRef) {
      this.mainResizeObserver.observe(this.actionsEditorRef);
    }
  }

  private initMainSize() {
    if (!this.contentRef || isFullscreen() || (isMobile() && !isIPad() && !isAndroidTablet())) {
      this.mainSize = {
        width: isMobile() ? 'calc(100% - 32px)' : '100%',
        height: isMobile() ? 'calc(100% - 32px)' : '100%',
      };
      return;
    }

    const maxHeight: number = this.contentRef.offsetHeight - 32;

    const wideScreen: MediaQueryList = window.matchMedia('(min-width: 1200px)');

    const width: number = this.contentRef.offsetWidth - (wideScreen.matches ? 192 : 32);
    const height: number = (width * 9) / 16;

    this.mainSize =
      height > maxHeight
        ? {
            width: `${(maxHeight * 16) / 9}px`,
            height: `${maxHeight}px`,
          }
        : {
            width: `${width}px`,
            height: `${height}px`,
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

      await this.deckRef?.slideTo(this.activeIndex);
    });

    this.slideResizeObserver.observe(this.mainRef);
  }

  private async initSlideSize() {
    await this.deckEventsHandler.initSlideSize();
  }

  @Listen('signIn', {target: 'document'})
  async signIn() {
    navigateSignIn();
  }

  private stickyToolbarActivated($event: CustomEvent) {
    this.hideActions = $event ? isMobile() && !isIOS() && $event.detail : false;
    this.hideNavigation = $event ? isIOS() && $event.detail : false;
  }

  @Listen('reorder', {target: 'document'})
  async onReorderSlides($event: CustomEvent<ItemReorderEventDetail>) {
    if (!$event || !$event.detail) {
      return;
    }

    await this.reorderSlides($event.detail);
  }

  private reorderSlides(detail: ItemReorderEventDetail): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!detail) {
        resolve();
        return;
      }

      try {
        await this.deckEventsHandler.updateDeckSlidesOrder(detail);

        if (detail.from < 0 || detail.to < 0 || !this.slides || detail.to >= this.slides.length || detail.from === detail.to) {
          resolve();
          return;
        }

        await this.remoteEventsHandler.updateRemoteSlidesOnMutation();

        this.slides.splice(detail.to, 0, ...this.slides.splice(detail.from, 1));
        this.slides = [...this.slides];
      } catch (err) {
        // We ignore the error here
      }

      // Finish the reorder and position the item in the DOM based on where the gesture ended. This method can also be called directly by the reorder group
      detail.complete();

      resolve();
    });
  }

  private async updatePresenting(presenting: boolean) {
    this.presenting = presenting;

    await this.remoteEventsHandler.updateRemoteReveal(this.fullscreen && this.presenting);

    await this.deckEventsHandler.toggleSlideEditable(!this.presenting);
  }

  @Listen('remoteSlideDidChange', {target: 'document'})
  async onRemoteSlideDidChange() {
    await this.onSlideChange();
  }

  private async onSlideChange() {
    await this.deckEventsHandler.toggleSlideEditable(!this.fullscreen || !this.presenting);

    const index: number = await this.deckRef?.getActiveIndex();

    if (index < 0 || this.activeIndex === index) {
      return;
    }

    this.activeIndex = index;
  }

  private async selectStep($event: CustomEvent<HTMLElement>) {
    this.actionsEditorRef?.selectStep($event?.detail);
  }

  render() {
    const autoSlide: boolean = deckStore.state.deck?.data?.attributes?.autoSlide !== undefined ? deckStore.state.deck.data.attributes.autoSlide : false;

    return [
      <app-navigation publish={true} class={this.hideNavigation ? 'hidden' : undefined}></app-navigation>,
      <ion-content class="ion-no-padding">
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
            </main>
          </div>

          <app-breadcrumbs slideNumber={this.activeIndex} onStepTo={($event: CustomEvent<HTMLElement>) => this.selectStep($event)}></app-breadcrumbs>

          <app-actions-editor
            ref={(el) => (this.actionsEditorRef = el as HTMLAppActionsEditorElement)}
            hideActions={this.hideActions}
            fullscreen={this.fullscreen}
            slides={this.slides}
            slideNumber={this.activeIndex}
            onSignIn={() => this.signIn()}
            onAddSlide={($event: CustomEvent<JSX.IntrinsicElements>) => this.addSlide($event)}
            onAnimatePrevNextSlide={($event: CustomEvent<boolean>) => this.animatePrevNextSlide($event)}
            onSlideTo={($event: CustomEvent<number>) => this.slideTo($event)}
            onSlideCopy={($event: CustomEvent<HTMLElement>) => this.copySlide($event)}
            onSlideTransform={($event: CustomEvent<JSX.IntrinsicElements>) => this.transformSlide($event)}
            onElementFocus={($event: CustomEvent<HTMLElement>) => this.onElementFocus($event)}
            onPresenting={($event: CustomEvent<boolean>) => this.updatePresenting($event?.detail)}></app-actions-editor>
        </div>
        <app-slide-preview deckRef={this.deckRef}></app-slide-preview>
        {this.renderLaserPointer()}
      </ion-content>,
      this.renderInlineEditor(),
    ];
  }

  private renderLaserPointer() {
    return this.fullscreen && this.presenting ? <deckgo-laser-pointer></deckgo-laser-pointer> : undefined;
  }

  private renderInlineEditor() {
    if (this.presenting) {
      return undefined;
    }

    return (
      <deckgo-inline-editor
        containers="h1,h2,h3,section,deckgo-reveal,deckgo-reveal-list,ol,ul"
        sticky-mobile="true"
        onStickyToolbarActivated={($event: CustomEvent) => this.stickyToolbarActivated($event)}
        img-anchor="deckgo-lazy-img"
        list={false}
        palette={colorStore.state.history}
        align={false}></deckgo-inline-editor>
    );
  }

  private renderLoading() {
    if (this.slidesFetched) {
      return undefined;
    } else {
      return <app-spinner></app-spinner>;
    }
  }
}
