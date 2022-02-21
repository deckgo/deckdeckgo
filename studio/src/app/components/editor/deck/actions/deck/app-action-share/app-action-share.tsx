import {editorStore} from '@deckdeckgo/studio';
import type {OverlayEventDetail} from '@ionic/core';
import {popoverController} from '@ionic/core';
import {Component, Element, Event, EventEmitter, h} from '@stencil/core';
import i18n from '../../../../../../stores/i18n.store';
import {MoreAction} from '../../../../../../types/editor/more-action';
import {cloud} from '../../../../../../utils/core/environment.utils';
import {share} from '../../../../../../utils/core/share.utils';
import {AppIcon} from '../../../../../core/app-icon/app-icon';

@Component({
  tag: 'app-action-share'
})
export class AppActionShare {
  @Element() el: HTMLElement;

  @Event() private actionPublish: EventEmitter<void>;

  @Event() private openEmbed: EventEmitter<void>;

  private cloud: boolean = cloud();

  private async share($event: UIEvent) {
    if (editorStore.default.state.published) {
      await this.openShareOptions($event);
      return;
    }

    this.actionPublish.emit();
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
          share();
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
    if (!this.cloud && !editorStore.default.state.published) {
      return undefined;
    }

    return (
      <button
        onMouseDown={($event) => $event.stopPropagation()}
        onTouchStart={($event) => $event.stopPropagation()}
        aria-label={i18n.state.editor.share}
        onClick={($event: UIEvent) => this.share($event)}
        class="ion-activatable"
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="share" ariaLabel="" ariaHidden={true}></AppIcon>
        <ion-label aria-hidden="true">{i18n.state.editor.share}</ion-label>
      </button>
    );
  }
}
