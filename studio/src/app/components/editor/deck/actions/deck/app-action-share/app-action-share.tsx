import {Component, Element, Event, EventEmitter, h} from '@stencil/core';

import type {OverlayEventDetail} from '@ionic/core';
import {popoverController} from '@ionic/core';

import deckStore from '../../../../../../stores/deck.store';
import userStore from '../../../../../../stores/user.store';
import shareStore from '../../../../../../stores/share.store';
import i18n from '../../../../../../stores/i18n.store';

import {MoreAction} from '../../../../../../types/editor/more-action';

import {AppIcon} from '../../../../../core/app-icon/app-icon';
import {share} from '../../../../../../utils/core/environment.utils';

@Component({
  tag: 'app-action-share'
})
export class AppActionShare {
  @Element() el: HTMLElement;

  @Event() private actionPublish: EventEmitter<void>;

  @Event() private openEmbed: EventEmitter<void>;

  private shareEnabled: boolean = share();

  private async share($event: UIEvent) {
    if (deckStore.state.published) {
      await this.openShareOptions($event);
    } else if (this.shareEnabled) {
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
      mode: 'ios'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        if (detail.data.action === MoreAction.SHARE) {
          shareStore.state.share = {
            deck: deckStore.state.deck,
            userName: userStore.state.name,
            userSocial: userStore.state.social
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
    if (!this.shareEnabled && !deckStore.state.published) {
      return undefined;
    }

    return (
      <button
        onMouseDown={($event) => $event.stopPropagation()}
        onTouchStart={($event) => $event.stopPropagation()}
        aria-label={i18n.state.editor.share}
        onClick={($event: UIEvent) => this.share($event)}
        class="ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="share" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.share}</ion-label>
      </button>
    );
  }
}
