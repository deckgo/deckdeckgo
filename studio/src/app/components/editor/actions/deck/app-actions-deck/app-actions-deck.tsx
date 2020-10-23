import {Component, Element, Event, EventEmitter, h, JSX, Prop, State} from '@stencil/core';
import {modalController, OverlayEventDetail, popoverController} from '@ionic/core';

import {isIPad} from '@deckdeckgo/utils';
import {ConnectionState, DeckdeckgoEventDeckRequest} from '@deckdeckgo/types';

import {get, set} from 'idb-keyval';

import offlineStore from '../../../../../stores/offline.store';
import remoteStore from '../../../../../stores/remote.store';
import deckStore from '../../../../../stores/deck.store';
import userStore from '../../../../../stores/user.store';
import shareStore from '../../../../../stores/share.store';

import {MoreAction} from '../../../../../utils/editor/more-action';

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
  deckDidChange: EventEmitter;

  @Event()
  private selectDeck: EventEmitter<void>;

  @State()
  private fullscreenEnable: boolean = true;

  private destroyListener;

  async componentWillLoad() {
    this.fullscreenEnable = !isIPad();

    this.destroyListener = remoteStore.onChange('pendingRequests', async (requests: DeckdeckgoEventDeckRequest[] | undefined) => {
      if (requests && requests.length > 0) {
        await this.clickToOpenRemote();
      }

      this.destroyListener();
    });
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
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
    if (!$event) {
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
          shareStore.state.share = {
            deck: deckStore.state.deck,
            userName: userStore.state.name,
            userSocial: userStore.state.social,
          };
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
      showBackdrop: true,
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
      let button: HTMLElement = this.el.querySelector('button.open-remote');

      if (!button) {
        return;
      }

      const style: CSSStyleDeclaration = window.getComputedStyle(button);

      // Actions are grouped in a popover on small devices?
      if (style.display === 'none') {
        button = this.el.querySelector('button.open-remote-small-devices');

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
          <app-action-add-slide slides={this.slides} blockSlide={this.blockSlide} signIn={this.signIn} addSlide={this.addSlide}></app-action-add-slide>

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            aria-label="Previous"
            onClick={() => this.animatePrevNextSlide.emit(false)}
            class="ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <ion-icon aria-hidden="true" src="/assets/icons/ionicons/arrow-back.svg"></ion-icon>
            <ion-label aria-hidden="true">Previous</ion-label>
          </button>

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            aria-label="Next"
            onClick={() => this.animatePrevNextSlide.emit(true)}
            class="ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <ion-icon aria-hidden="true" src="/assets/icons/ionicons/arrow-forward.svg"></ion-icon>
            <ion-label aria-hidden="true">Next</ion-label>
          </button>

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            aria-label="Slides"
            onClick={() => this.openSlideNavigate()}
            color="primary"
            class="ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <ion-icon aria-hidden="true" src="/assets/icons/ionicons/md-list.svg"></ion-icon>
            <ion-label aria-hidden="true">Slides</ion-label>
          </button>

          <app-action-busy aria-label="Style" iconSrc="/assets/icons/ionicons/brush.svg" class="wider-devices" onActionReady={() => this.openDeckStyle()}>
            <ion-label aria-hidden="true">Style</ion-label>
          </app-action-busy>

          {this.renderFullscreenButton()}

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            aria-label="Remote"
            onClick={($event: UIEvent) => this.openRemote($event)}
            color="primary"
            class="wider-devices open-remote ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <ion-icon aria-hidden="true" src="/assets/icons/ionicons/phone-portrait.svg"></ion-icon>
            <ion-label aria-hidden="true">Remote</ion-label>
          </button>
        </ion-buttons>

        <ion-buttons slot="end">
          <app-action-share class="wider-devices" onOpenEmbed={() => this.openEmbed()}></app-action-share>

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            aria-label={offlineStore.state.offline ? 'Go online' : 'Go offline'}
            onClick={() => this.goOnlineOffline()}
            color="primary"
            class="wider-devices ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <ion-icon aria-hidden="true" src={`/assets/icons/ionicons/${offlineStore.state.offline ? 'cloud-done' : 'cloud-offline'}.svg`}></ion-icon>
            {offlineStore.state.offline ? <ion-label aria-hidden="true">Go online</ion-label> : <ion-label aria-hidden="true">Go offline</ion-label>}
          </button>

          <app-action-help class="wider-devices"></app-action-help>

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            aria-label="Remote"
            onClick={($event: UIEvent) => this.openRemote($event)}
            color="primary"
            class="small-devices open-remote-small-devices ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <ion-icon aria-hidden="true" src="/assets/icons/ionicons/phone-portrait.svg"></ion-icon>
            <ion-label aria-hidden="true">Remote</ion-label>
          </button>

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            onClick={(e: UIEvent) => this.openMoreActions(e)}
            color="primary"
            class="small-devices ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <ion-icon aria-hidden="true" src="/assets/icons/ionicons/ellipsis-vertical.svg"></ion-icon>
            <ion-label aria-hidden="true">More</ion-label>
          </button>
        </ion-buttons>
      </ion-toolbar>
    );
  }

  private renderFullscreenButton() {
    if (this.fullscreenEnable) {
      return (
        <button
          onMouseDown={($event) => $event.stopPropagation()}
          onTouchStart={($event) => $event.stopPropagation()}
          onClick={() => this.toggleFullScreenMode()}
          color="primary"
          class="wider-devices ion-activatable">
          <ion-ripple-effect></ion-ripple-effect>
          {this.renderFullscreen()}
        </button>
      );
    } else {
      return undefined;
    }
  }

  private renderFullscreen() {
    if (this.fullscreen) {
      return [<ion-icon aria-hidden="true" src="/assets/icons/ionicons/contract.svg"></ion-icon>, <ion-label aria-hidden="true">Exit fullscreen</ion-label>];
    } else {
      return [<ion-icon aria-hidden="true" src="/assets/icons/ionicons/expand.svg"></ion-icon>, <ion-label aria-hidden="true">Fullscreen</ion-label>];
    }
  }
}
