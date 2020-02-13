import {Component, Element, Listen, Prop, State, h, JSX} from '@stencil/core';

import {ItemReorderEventDetail, modalController, OverlayEventDetail} from '@ionic/core';

import {Subscription} from 'rxjs';
import {filter, take} from 'rxjs/operators';

import {isFullscreen, isMobile, debounce, isIOS} from '@deckdeckgo/utils';

import {convertStyle} from '@deckdeckgo/deck-utils';

import {generateRandomStyleColors} from '../../../utils/editor/random-palette';

import {AuthUser} from '../../../models/auth/auth.user';
import {SlideTemplate} from '../../../models/data/slide';
import {Deck} from '../../../models/data/deck';

import {CreateSlidesUtils} from '../../../utils/editor/create-slides.utils';
import {ParseBackgroundUtils} from '../../../utils/editor/parse-background.utils';

import {DeckEventsHandler} from '../../../handlers/editor/events/deck/deck-events.handler';
import {RemoteEventsHandler} from '../../../handlers/editor/events/remote/remote-events.handler';
import {EditorEventsHandler} from '../../../handlers/editor/events/editor/editor-events.handler';
import {PollEventsHandler} from '../../../handlers/editor/events/poll/poll-events.handler';

import {EditorHelper} from '../../../helpers/editor/editor.helper';

import {ParseElementsUtils} from '../../../utils/editor/parse-elements.utils';
import {SlotType} from '../../../utils/editor/slot-type';
import {SlotUtils} from '../../../utils/editor/slot.utils';
import {FontsUtils} from '../../../utils/editor/fonts.utils';

import {AuthService} from '../../../services/auth/auth.service';
import {AnonymousService} from '../../../services/editor/anonymous/anonymous.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';
import {BusyService} from '../../../services/editor/busy/busy.service';

import {EnvironmentGoogleConfig} from '../../../services/core/environment/environment-config';
import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';

@Component({
  tag: 'app-editor',
  styleUrl: 'app-editor.scss'
})
export class AppEditor {
  @Element() el: HTMLElement;

  @Prop()
  deckId: string;

  @State()
  private slides: JSX.IntrinsicElements[] = [];

  @State()
  private background: any;

  @State()
  private style: any;

  @State()
  private transition: 'slide' | 'fade' | 'none' = 'slide';

  private slideIndex: number = 0;

  @State()
  private presenting: boolean = false;

  private deckEventsHandler: DeckEventsHandler = new DeckEventsHandler();
  private remoteEventsHandler: RemoteEventsHandler = new RemoteEventsHandler();
  private editorEventsHandler: EditorEventsHandler = new EditorEventsHandler();
  private pollEventsHandler: PollEventsHandler = new PollEventsHandler();

  private editorHelper: EditorHelper = new EditorHelper();

  private authService: AuthService;
  private anonymousService: AnonymousService;
  private navService: NavService;

  private deckEditorService: DeckEditorService;

  private busySubscription: Subscription;
  private busyService: BusyService;

  @State()
  private slidesFetched: boolean = false;

  @State()
  private hideFooterActions: boolean = true;

  @State()
  private hideNavigation: boolean = false;

  @State()
  private slidesEditable: boolean = false;

  @State()
  private fullscreen: boolean = false;

  constructor() {
    this.authService = AuthService.getInstance();
    this.anonymousService = AnonymousService.getInstance();
    this.navService = NavService.getInstance();
    this.deckEditorService = DeckEditorService.getInstance();
    this.busyService = BusyService.getInstance();
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
    await this.deckEventsHandler.init(this.el);
    await this.editorEventsHandler.init(this.el);

    // If no user create an anonymous one
    this.authService
      .watch()
      .pipe(take(1))
      .subscribe(async (authUser: AuthUser) => {
        if (!authUser) {
          await this.authService.signInAnonymous();
        }
      });

    // As soon as we have got a user, an anonymous where the creation started above or an already used anonymous or a logged one, we init
    this.authService
      .watch()
      .pipe(
        filter((authUser: AuthUser) => authUser !== null && authUser !== undefined),
        take(1)
      )
      .subscribe(async (_authUser: AuthUser) => {
        if (!this.deckId) {
          await this.initSlide();
        } else {
          await this.fetchSlides();
        }

        this.slidesFetched = true;
      });

    this.busySubscription = this.busyService.watchSlideEditable().subscribe(async (slide: HTMLElement) => {
      // Hide actions footer till deck is editable
      this.hideFooterActions = false;

      this.slidesEditable = true;

      await this.contentEditable(slide);
    });

    this.fullscreen = isFullscreen();
  }

  async destroy() {
    this.deckEventsHandler.destroy();
    this.editorEventsHandler.destroy();
    this.pollEventsHandler.destroy();

    await this.remoteEventsHandler.destroy();

    if (this.busySubscription) {
      this.busySubscription.unsubscribe();
    }

    this.deckEditorService.next(null);
  }

  async componentDidLoad() {
    await this.updateInlineEditorListener();

    await this.remoteEventsHandler.init(this.el);
    await this.pollEventsHandler.init(this.el);

    this.initWindowResize();
  }

  async componentDidUnload() {
    await this.remoteEventsHandler.destroy();

    this.removeWindowResize();
  }

  private updateInlineEditorListener(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLElement = this.el.querySelector('deckgo-deck');

      if (!deck) {
        return;
      }

      const inlineEditor: HTMLElement = this.el.querySelector('deckgo-inline-editor');

      if (!inlineEditor) {
        return;
      }

      (inlineEditor as any).attachTo = deck;

      resolve();
    });
  }

  private initSlide(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!document) {
        resolve();
        return;
      }

      const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide(SlideTemplate.TITLE);

      await this.concatSlide(slide);

      await this.initRandomDeckStyle();

      resolve();
    });
  }

  private fetchSlides(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!document || !this.deckId) {
        resolve();
        return;
      }

      const slides: any[] = await this.editorHelper.loadDeckAndRetrieveSlides(this.deckId);

      if (slides && slides.length > 0) {
        this.slides = [...slides];
      }

      await this.initDeckStyle();

      resolve();
    });
  }

  private async initRandomDeckStyle() {
    this.style = await generateRandomStyleColors();
  }

  private initDeckStyle(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.deckEditorService
        .watch()
        .pipe(take(1))
        .subscribe(async (deck: Deck) => {
          if (deck && deck.data && deck.data.attributes && deck.data.attributes.style) {
            this.style = await convertStyle(deck.data.attributes.style);
          } else {
            this.style = undefined;
          }

          if (deck && deck.data && deck.data.attributes && deck.data.attributes.transition) {
            this.transition = deck.data.attributes.transition;
          }

          this.background = await ParseBackgroundUtils.convertBackground(deck.data.background, true);

          const google: EnvironmentGoogleConfig = EnvironmentConfigService.getInstance().get('google');
          await FontsUtils.loadGoogleFont(google.fontsUrl, this.style);

          resolve();
        });
    });
  }

  private concatSlide(extraSlide: JSX.IntrinsicElements): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.slides = [...this.slides, extraSlide];

      await ParseBackgroundUtils.stickDeckBackgroundLastChild(this.el);

      resolve();
    });
  }

  async inactivity($event: CustomEvent) {
    await this.updatePresenting(!$event.detail);

    if (!this.presenting) {
      await this.hideToolbar();
    }

    // Wait a bit for the display/repaint of the footer
    setTimeout(async () => {
      await this.deckEventsHandler.initSlideSize();
    }, 100);
  }

  private async animatePrevNextSlide($event: CustomEvent<boolean>) {
    if (!$event) {
      return;
    }

    const deck: HTMLElement = this.el.querySelector('deckgo-deck');

    if (!deck) {
      return;
    }

    if ($event.detail) {
      await (deck as any).slideNext(false, true);
    } else {
      await (deck as any).slidePrev(false, true);
    }
  }

  private async slideTo($event: CustomEvent<number>) {
    if (!$event) {
      return;
    }

    const deck: HTMLElement = this.el.querySelector('deckgo-deck');

    if (!deck) {
      return;
    }

    await (deck as any).slideTo($event.detail);
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
      cssClass: 'fullscreen'
    });

    modal.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
      await this.editorEventsHandler.blockSlide(false);
    });

    await modal.present();
  }

  private hideToolbar(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const actions: HTMLAppActionsEditorElement = this.el.querySelector('app-actions-editor');

      if (!actions) {
        resolve();
        return;
      }

      await actions.hide();

      resolve();
    });
  }

  private onSlideChangeHideToolbar(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const actions: HTMLAppActionsEditorElement = this.el.querySelector('app-actions-editor');

      if (!actions) {
        resolve();
        return;
      }

      const deck: HTMLElement = this.el.querySelector('deckgo-deck');

      if (!deck) {
        resolve();
        return;
      }

      const deckIndex: number = await (deck as any).getActiveIndex();

      if (deckIndex === this.slideIndex) {
        resolve();
        return;
      }

      await actions.hide();
      this.slideIndex = deckIndex;

      resolve();
    });
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

      if (!selectedElement.nodeName || selectedElement.nodeName.toLowerCase().indexOf('deckgo-slide') >= 0) {
        resolve();
        return;
      }

      const slide: HTMLElement = selectedElement.parentElement;

      if (!slide || !slide.parentNode) {
        resolve();
        return;
      }

      const selectedElementSlideIndex: number = Array.prototype.indexOf.call(slide.parentNode.children, slide);

      if (selectedElementSlideIndex === this.slideIndex) {
        resolve();
        return;
      }

      const deck: HTMLElement = this.el.querySelector('deckgo-deck');

      if (!deck || !deck.parentElement) {
        resolve();
        return;
      }

      await (deck as any).slideTo(selectedElementSlideIndex);

      // Selecting an element with "Tab" outside of what's displayed in the viewport will cause a scroll on the parent element, <main/>.
      // This is applied by the browser. Therefore we set it back to the origin, as we takes care of the positioning of the slider.
      deck.parentElement.scrollTo(0, 0);

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

  private touchToolbar(element: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const actions: HTMLAppActionsEditorElement = this.el.querySelector('app-actions-editor');

      if (!actions) {
        resolve();
        return;
      }

      await actions.touch(element);

      resolve();
    });
  }

  private toggleFullScreen(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLElement = this.el.querySelector('deckgo-deck');

      if (!deck) {
        resolve();
        return;
      }

      await this.editorEventsHandler.selectDeck();
      await (deck as any).toggleFullScreen();

      resolve();
    });
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
    this.fullscreen = isFullscreen();

    // Per default, when we switch to the fullscreen mode, we want to present the presentation not edit it
    await this.updatePresenting(this.fullscreen);
  };

  @Listen('signIn', {target: 'document'})
  async signIn() {
    this.navService.navigate({
      url: '/signin' + (window && window.location ? window.location.pathname : ''),
      direction: NavDirection.FORWARD
    });
  }

  private contentEditable(slide: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!slide || slide.childElementCount <= 0) {
        resolve();
        return;
      }

      const elements: HTMLElement[] = Array.prototype.slice.call(slide.childNodes);
      elements.forEach((e: HTMLElement) => {
        if (e.nodeName && e.nodeType === 1 && e.hasAttribute('slot')) {
          if (e.nodeName.toLowerCase() === SlotType.CODE) {
            e.setAttribute('editable', '');
          } else if (ParseElementsUtils.isElementContentEditable(e)) {
            e.setAttribute('contentEditable', '');
          } else if (SlotUtils.isNodeReveal(e) && e.firstElementChild) {
            e.firstElementChild.setAttribute('contentEditable', '');
          }
        }
      });

      resolve();
    });
  }

  private stickyToolbarActivated($event: CustomEvent) {
    this.hideFooterActions = $event ? $event.detail : false;
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
  }

  render() {
    return [
      <app-navigation publish={true} class={this.hideNavigation ? 'hidden' : undefined}></app-navigation>,
      <ion-content>
        <main class={this.slidesEditable ? (this.presenting ? 'ready idle' : 'ready') : undefined}>
          {this.renderLoading()}

          <deckgo-deck
            embedded={true}
            style={this.style}
            reveal={this.fullscreen && this.presenting}
            transition={this.transition}
            onMouseDown={(e: MouseEvent) => this.deckTouched(e)}
            onTouchStart={(e: TouchEvent) => this.deckTouched(e)}
            onSlideNextDidChange={() => this.onSlideChangeHideToolbar()}
            onSlidePrevDidChange={() => this.onSlideChangeHideToolbar()}
            onSlideToChange={() => this.onSlideChangeHideToolbar()}
            onMouseInactivity={($event: CustomEvent) => this.inactivity($event)}>
            {this.slides}
            {this.background}
          </deckgo-deck>
          <deckgo-remote autoConnect={false}></deckgo-remote>
        </main>
      </ion-content>,
      <ion-footer class={this.presenting ? 'idle' : undefined}>
        <app-actions-editor
          hideFooterActions={this.hideFooterActions}
          fullscreen={this.fullscreen}
          slides={this.slides}
          onSignIn={() => this.signIn()}
          onAddSlide={($event: CustomEvent<JSX.IntrinsicElements>) => this.addSlide($event)}
          onAnimatePrevNextSlide={($event: CustomEvent<boolean>) => this.animatePrevNextSlide($event)}
          onSlideTo={($event: CustomEvent<number>) => this.slideTo($event)}
          onToggleFullScreen={() => this.toggleFullScreen()}
          onSlideCopy={($event: CustomEvent<HTMLElement>) => this.copySlide($event)}
          onElementFocus={($event: CustomEvent<HTMLElement>) => this.onElementFocus($event)}></app-actions-editor>
      </ion-footer>,
      <deckgo-inline-editor
        containers="h1,h2,h3,section,deckgo-reveal,deckgo-reveal-list,ol,ul"
        sticky-mobile="true"
        onStickyToolbarActivated={($event: CustomEvent) => this.stickyToolbarActivated($event)}
        img-anchor="deckgo-lazy-img"
        list={false}></deckgo-inline-editor>
    ];
  }

  private renderLoading() {
    if (this.slidesFetched) {
      return undefined;
    } else {
      return (
        <div class="spinner">
          <ion-spinner color="primary"></ion-spinner>
        </div>
      );
    }
  }
}
