import {Component, Element, Event, EventEmitter, h, JSX, Prop} from '@stencil/core';

import type {OverlayEventDetail} from '@ionic/core';

import {ConnectionState, DeckdeckgoEventDeckRequest} from '@deckdeckgo/types';

import offlineStore from '../../../../../stores/offline.store';
import remoteStore from '../../../../../stores/remote.store';
import deckStore from '../../../../../stores/deck.store';
import userStore from '../../../../../stores/user.store';
import shareStore from '../../../../../stores/share.store';
import i18n from '../../../../../stores/i18n.store';

import {MoreAction} from '../../../../../types/editor/more-action';

import { modalController, popoverController } from '../../../../../utils/ionic/ionic.overlay';

import { AppIcon } from '../../../../core/app-icon/app-icon';

@Component({
  tag: 'app-actions-deck',
  shadow: false
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

  private destroyListener;

  async componentWillLoad() {
    this.destroyListener = remoteStore.onChange('pendingRequests', async (requests: DeckdeckgoEventDeckRequest[] | undefined) => {
      if (requests && requests.length > 0) {
        await this.openRemoteControlRequest();
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
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-slide-navigate',
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu popover-menu-wide'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data >= 0) {
        this.slideTo.emit(detail.data);
      }
    });

    await popover.present();
  }

  private async openRemoteControlRequest() {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-remote-request',
      mode: 'ios',
      cssClass: 'info'
    });

    await popover.present();
  }

  private async openRemoteControlConnect() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-remote-connect'
    });

    await modal.present();
  }

  private async openEmbed() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-embed'
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
        offline: offlineStore.state.offline
      },
      event: $event,
      mode: 'ios'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        if (detail.data.action === MoreAction.PRESENT) {
          await this.openPresent();
        } else if (detail.data.action === MoreAction.JUMP_TO) {
          await this.openSlideNavigate();
        } else if (detail.data.action === MoreAction.SHARE) {
          shareStore.state.share = {
            deck: deckStore.state.deck,
            userName: userStore.state.name,
            userSocial: userStore.state.social
          };
        } else if (detail.data.action === MoreAction.PUBLISH) {
          this.actionPublish.emit();
        } else if (detail.data.action === MoreAction.EMBED) {
          await this.openEmbed();
        }
      }
    });

    await popover.present();
  }

  private async toggleFullScreenMode() {
    this.toggleFullScreen.emit();
  }

  async openDeckStyle() {
    this.selectDeck.emit();

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-deck-style',
      componentProps: {
        signIn: this.signIn,
        blockSlide: this.blockSlide,
        deckDidChange: this.deckDidChange
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu popover-menu-wide'
    });

    await popover.present();
  }

  private async openRemote() {
    const connected: boolean =
      remoteStore.state.connectionState !== ConnectionState.DISCONNECTED && remoteStore.state.connectionState !== ConnectionState.NOT_CONNECTED;

    if (connected && remoteStore.state.pendingRequests && remoteStore.state.pendingRequests.length > 0) {
      await this.closeRemote();
      await this.openRemoteControlRequest();
    } else {
      await this.openRemoteControlConnect();
    }
  }

  private async openPresent() {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-present',
      componentProps: {
        fullscreen: this.fullscreen
      },
      mode: 'ios',
      showBackdrop: false,
      cssClass: 'popover-menu'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail?.data?.action === MoreAction.REMOTE) {
        await this.openRemote();
      }
    });

    await popover.present();
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
      <aside>
        <ion-buttons slot="start">
          <app-action-add-slide slides={this.slides} blockSlide={this.blockSlide} signIn={this.signIn} addSlide={this.addSlide}></app-action-add-slide>

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            aria-label={i18n.state.editor.previous}
            onClick={() => this.animatePrevNextSlide.emit(false)}
            class="ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <AppIcon name="arrow-back" ariaLabel="" ariaHidden={true}></AppIcon>
            <ion-label aria-hidden="true">{i18n.state.editor.previous}</ion-label>
          </button>

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            aria-label={i18n.state.editor.next}
            onClick={() => this.animatePrevNextSlide.emit(true)}
            class="ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <AppIcon name="arrow-forward" ariaLabel="" ariaHidden={true}></AppIcon>
            <ion-label aria-hidden="true">{i18n.state.editor.next}</ion-label>
          </button>

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            aria-label={i18n.state.editor.slides}
            onClick={() => this.openSlideNavigate()}
            color="primary"
            class="ion-activatable wider-devices">
            <ion-ripple-effect></ion-ripple-effect>
            <AppIcon name="md-list" ariaLabel="" ariaHidden={true}></AppIcon>
            <ion-label aria-hidden="true">{i18n.state.editor.slides}</ion-label>
          </button>

          <app-action-busy aria-label="Style" iconName="brush" onActionReady={() => this.openDeckStyle()}>
            <ion-label aria-hidden="true">{i18n.state.editor.style}</ion-label>
          </app-action-busy>
        </ion-buttons>

        <ion-buttons slot="end">
          {this.renderExitFullscreenButton()}

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            aria-label={i18n.state.editor.present}
            onClick={() => this.openPresent()}
            color="primary"
            class="wider-devices open-remote ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <AppIcon name="play" ariaLabel="" ariaHidden={true}></AppIcon>
            <ion-label aria-hidden="true">{i18n.state.editor.present}</ion-label>
          </button>

          <app-action-share class="wider-devices" onOpenEmbed={() => this.openEmbed()}></app-action-share>

          <app-action-help class="wider-devices"></app-action-help>

          <button
            onMouseDown={($event) => $event.stopPropagation()}
            onTouchStart={($event) => $event.stopPropagation()}
            onClick={(e: UIEvent) => this.openMoreActions(e)}
            color="primary"
            class="small-devices ion-activatable">
            <ion-ripple-effect></ion-ripple-effect>
            <AppIcon name="ellipsis-vertical" ariaLabel="" ariaHidden={true}></AppIcon>
            <ion-label aria-hidden="true">{i18n.state.editor.more}</ion-label>
          </button>
        </ion-buttons>
      </aside>
    );
  }

  private renderExitFullscreenButton() {
    if (!this.fullscreen) {
      return undefined;
    }

    return (
      <button
        onMouseDown={($event) => $event.stopPropagation()}
        onTouchStart={($event) => $event.stopPropagation()}
        onClick={() => this.toggleFullScreenMode()}
        color="primary"
        class="wider-devices ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="contract" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.exit_fullscreen}</ion-label>
      </button>
    );
  }
}
