import {Component, Element, Event, EventEmitter, h, JSX, Listen, Prop, State} from '@stencil/core';
import {modalController, OverlayEventDetail, popoverController} from '@ionic/core';

import {isIPad} from '@deckdeckgo/utils';
import {ConnectionState, DeckdeckgoEventDeckRequest} from '@deckdeckgo/types';

import {get, set} from 'idb-keyval';

import offlineStore from '../../../../../stores/offline.store';
import remoteStore from '../../../../../stores/remote.store';

import {MoreAction} from '../../../../../utils/editor/more-action';

import {SlidesHelper} from '../../../../../helpers/editor/slides.helper';

import {AnonymousService} from '../../../../../services/editor/anonymous/anonymous.service';

@Component({
  tag: 'app-actions-deck',
  shadow: false,
})
export class AppActionsDeck {
  @Element() el: HTMLElement;

  @Prop()
  fullscreen: boolean = false;

  @Prop()
  slides: JSX.IntrinsicElements[] = [];

  @Prop()
  blockSlide: EventEmitter;

  @Prop()
  signIn: EventEmitter;

  @Prop()
  addSlide: EventEmitter;

  @Prop()
  animatePrevNextSlide: EventEmitter;

  @Prop()
  slideTo: EventEmitter;

  @Prop()
  toggleFullScreen: EventEmitter;

  @Prop()
  actionPublish: EventEmitter;

  @Prop()
  openShare: EventEmitter;

  @Prop()
  deckDidChange: EventEmitter;

  @Event()
  private selectDeck: EventEmitter<void>;

  @State()
  private fullscreenEnable: boolean = true;

  private anonymousService: AnonymousService;

  private destroyListener;

  constructor() {
    this.anonymousService = AnonymousService.getInstance();
  }

  async componentWillLoad() {
    this.fullscreenEnable = !isIPad();

    this.destroyListener = remoteStore.onChange('pendingRequests', async (requests: DeckdeckgoEventDeckRequest[] | undefined) => {
      if (requests && requests.length > 0) {
        await this.clickToOpenRemote();
      }

      this.destroyListener();
    });
  }

  componentDidUnload() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  private async onActionOpenSlideAdd($event: CustomEvent) {
    const couldAddSlide: boolean = await this.anonymousService.couldAddSlide(this.slides);

    if (!couldAddSlide) {
      this.signIn.emit();
      return;
    }

    const helper: SlidesHelper = new SlidesHelper(this.addSlide, this.blockSlide);
    await helper.openSlideAdd($event);
  }

  @Listen('pagerClick', {target: 'document'})
  async onPagerClick() {
    await this.openSlideNavigate();
  }

  private async openSlideNavigate() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-slide-navigate',
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data >= 0) {
        this.slideTo.emit(detail.data);
      }
    });

    await modal.present();
  }

  private async openRemoteControl($event: UIEvent, component: string) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: component,
      event: $event,
      mode: 'ios',
      cssClass: 'info',
    });

    await popover.present();
  }

  private async openEmbed() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-embed',
    });

    await modal.present();
  }

  private async openMoreActions($event: UIEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-more-deck-actions',
      componentProps: {
        offline: offlineStore.state.offline,
      },
      event: $event,
      mode: 'ios',
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        if (detail.data.action === MoreAction.FULLSCREEN) {
          await this.toggleFullScreenMode();
        } else if (detail.data.action === MoreAction.JUMP_TO) {
          await this.openSlideNavigate();
        } else if (detail.data.action === MoreAction.SHARE) {
          this.openShare.emit();
        } else if (detail.data.action === MoreAction.PUBLISH) {
          this.actionPublish.emit();
        } else if (detail.data.action === MoreAction.STYLE) {
          await this.openDeckStyle();
        } else if (detail.data.action === MoreAction.EMBED) {
          await this.openEmbed();
        } else if (detail.data.action === MoreAction.OFFLINE) {
          await this.goOnlineOffline();
        }
      }
    });

    await popover.present();
  }

  private toggleFullScreenMode(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.toggleFullScreen.emit();

      await this.openFullscreenInfo();

      resolve();
    });
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
    });

    popover.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
      await set('deckdeckgo_display_fullscreen_info', true);
    });

    await popover.present();
  }

  async openDeckStyle() {
    this.selectDeck.emit();

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-deck-style',
      componentProps: {
        signIn: this.signIn,
        blockSlide: this.blockSlide,
        deckDidChange: this.deckDidChange,
      },
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu popover-menu-wide',
    });

    await popover.present();
  }

  private async goOnlineOffline() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-offline',
      componentProps: {
        offline: offlineStore.state.offline,
      },
      cssClass: 'fullscreen',
    });

    await modal.present();
  }

  private async clickToOpenRemote() {
    if (remoteStore.state.connectionState !== ConnectionState.CONNECTED) {
      let button: HTMLElement = this.el.querySelector('ion-tab-button.open-remote');

      if (!button) {
        return;
      }

      const style: CSSStyleDeclaration = window.getComputedStyle(button);

      // Actions are grouped in a popover on small devices?
      if (style.display === 'none') {
        button = this.el.querySelector('ion-tab-button.open-remote-small-devices');

        if (!button) {
          return;
        }
      }

      // We click to button as we want to pass $event to the popover to stick it next to the button
      button.click();
    }
  }

  private async openRemote($event: UIEvent) {
    const connected: boolean =
      remoteStore.state.connectionState !== ConnectionState.DISCONNECTED && remoteStore.state.connectionState !== ConnectionState.NOT_CONNECTED;

    if (connected && remoteStore.state.pendingRequests && remoteStore.state.pendingRequests.length > 0) {
      await this.closeRemote();
      await this.openRemoteControl($event, 'app-remote-request');
    } else {
      await this.openRemoteControl($event, 'app-remote-connect');
    }
  }

  private async closeRemote() {
    if (!document) {
      return;
    }

    const popover: HTMLIonPopoverElement = document.querySelector('ion-popover');

    if (popover) {
      await popover.dismiss();
    }
  }

  render() {
    return (
      <ion-toolbar>
        <ion-buttons slot="start">
          <app-action-busy iconSrc="/assets/icons/ionicons/add.svg" onActionReady={($event: CustomEvent) => this.onActionOpenSlideAdd($event)}>
            <ion-label>Add slide</ion-label>
          </app-action-busy>

          <ion-tab-button onClick={() => this.animatePrevNextSlide.emit(false)} color="primary" mode="md">
            <ion-icon src="/assets/icons/ionicons/arrow-back.svg"></ion-icon>
            <ion-label>Previous</ion-label>
          </ion-tab-button>

          <ion-tab-button onClick={() => this.animatePrevNextSlide.emit(true)} color="primary" mode="md">
            <ion-icon src="/assets/icons/ionicons/arrow-forward.svg"></ion-icon>
            <ion-label>Next</ion-label>
          </ion-tab-button>

          <ion-tab-button onClick={() => this.openSlideNavigate()} color="primary" class="wider-devices" mode="md">
            <ion-icon src="/assets/icons/ionicons/md-list.svg"></ion-icon>
            <ion-label>Slides</ion-label>
          </ion-tab-button>

          <app-action-busy iconSrc="/assets/icons/ionicons/brush.svg" class="wider-devices" onActionReady={() => this.openDeckStyle()}>
            <ion-label>Style</ion-label>
          </app-action-busy>

          {this.renderFullscreenButton()}

          <ion-tab-button onClick={($event: UIEvent) => this.openRemote($event)} color="primary" class="wider-devices open-remote" mode="md">
            <ion-icon src="/assets/icons/ionicons/phone-portrait.svg"></ion-icon>
            <ion-label>Remote</ion-label>
          </ion-tab-button>
        </ion-buttons>

        <ion-buttons slot="end">
          <app-action-share class="wider-devices" onOpenEmbed={() => this.openEmbed()}></app-action-share>

          <ion-tab-button onClick={() => this.goOnlineOffline()} color="primary" class="wider-devices" mode="md">
            <ion-icon src={`/assets/icons/ionicons/${offlineStore.state.offline ? 'cloud-done' : 'cloud-offline'}.svg`}></ion-icon>
            {offlineStore.state.offline ? <ion-label>Go online</ion-label> : <ion-label>Go offline</ion-label>}
          </ion-tab-button>

          <app-action-help class="wider-devices"></app-action-help>

          <ion-tab-button onClick={($event: UIEvent) => this.openRemote($event)} color="primary" class="small-devices open-remote-small-devices" mode="md">
            <ion-icon src="/assets/icons/ionicons/phone-portrait.svg"></ion-icon>
            <ion-label>Remote</ion-label>
          </ion-tab-button>

          <ion-tab-button onClick={(e: UIEvent) => this.openMoreActions(e)} color="primary" class="small-devices" mode="md">
            <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg"></ion-icon>
            <ion-label>More</ion-label>
          </ion-tab-button>
        </ion-buttons>
      </ion-toolbar>
    );
  }

  private renderFullscreenButton() {
    if (this.fullscreenEnable) {
      return (
        <ion-tab-button onClick={() => this.toggleFullScreenMode()} color="primary" class="wider-devices" mode="md">
          {this.renderFullscreen()}
        </ion-tab-button>
      );
    } else {
      return undefined;
    }
  }

  private renderFullscreen() {
    if (this.fullscreen) {
      return [<ion-icon src="/assets/icons/ionicons/contract.svg"></ion-icon>, <ion-label>Exit fullscreen</ion-label>];
    } else {
      return [<ion-icon src="/assets/icons/ionicons/expand.svg"></ion-icon>, <ion-label>Fullscreen</ion-label>];
    }
  }
}
