import {Component, Element, Event, EventEmitter, h} from '@stencil/core';
import {OverlayEventDetail, popoverController} from '@ionic/core';

import store from '../../../../../stores/deck.store';

import {MoreAction} from '../../../../../utils/editor/more-action';

@Component({
  tag: 'app-action-share',
})
export class AppActionShare {
  @Element() el: HTMLElement;

  @Event() private actionPublish: EventEmitter<void>;

  @Event() private openShare: EventEmitter<void>;

  @Event() private openEmbed: EventEmitter<void>;

  private async share($event: UIEvent) {
    if (store.state.published) {
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
          this.openShare.emit();
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
      <ion-tab-button onClick={($event: UIEvent) => this.share($event)} color="primary" mode="md">
        <ion-icon src="/assets/icons/ionicons/share.svg"></ion-icon>
        <ion-label>Share</ion-label>
      </ion-tab-button>
    );
  }
}
