import {Component, Element, Event, EventEmitter, h} from '@stencil/core';
import {OverlayEventDetail, popoverController} from '@ionic/core';

import deckStore from '../../../../../stores/deck.store';
import userStore from '../../../../../stores/user.store';
import shareStore from '../../../../../stores/share.store';

import {MoreAction} from '../../../../../utils/editor/more-action';

@Component({
  tag: 'app-action-share',
})
export class AppActionShare {
  @Element() el: HTMLElement;

  @Event() private actionPublish: EventEmitter<void>;

  @Event() private openEmbed: EventEmitter<void>;

  private async share($event: UIEvent) {
    if (deckStore.state.published) {
      await this.openShareOptions($event);
    } else {
      this.actionPublish.emit();
    }
  }

  async openShareOptions($event: UIEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-more-share-options',
      event: $event,
      mode: 'ios',
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        if (detail.data.action === MoreAction.SHARE) {
          shareStore.state.share = {
            deck: deckStore.state.deck,
            userName: userStore.state.name,
            userSocial: userStore.state.social,
          };
        } else if (detail.data.action === MoreAction.PUBLISH) {
          this.actionPublish.emit();
        } else if (detail.data.action === MoreAction.EMBED) {
          this.openEmbed.emit();
        }
      }
    });

    await popover.present();
  }

  render() {
    return (
      <button aria-label="Share" onClick={($event: UIEvent) => this.share($event)} class="ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src="/assets/icons/ionicons/share.svg"></ion-icon>
        <ion-label aria-hidden="true">Share</ion-label>
      </button>
    );
  }
}
