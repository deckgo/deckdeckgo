import {Component, Event, EventEmitter, h, Prop, Host, State} from '@stencil/core';

import type {OverlayEventDetail} from '@ionic/core';
import {loadingController, popoverController} from '@ionic/core';

import {Deck} from '@deckdeckgo/editor';

import errorStore from '../../../stores/error.store';
import i18n from '../../../stores/i18n.store';

import {clone} from '../../../utils/core/dashboard.utils';

import {deleteDeck} from '../../../providers/data/deck/deck.provider';

import {AppIcon} from '../../core/app-icon/app-icon';

@Component({
  tag: 'app-dashboard-deck-actions',
  styleUrl: 'app-dashboard-deck-actions.scss'
})
export class AppDashboardDeckActions {
  @Prop()
  deck: Deck;

  @Prop()
  disableDelete: boolean;

  @Event() deckDeleted: EventEmitter<string>;
  @Event() deckCloned: EventEmitter<void>;

  @State()
  private actionInProgress: boolean = false;

  private async presentConfirmDelete($event: UIEvent) {
    $event.stopPropagation();

    if (this.actionInProgress) {
      return;
    }

    if (!this.deck || !this.deck.data) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-delete',
      event: $event,
      mode: 'ios'
    });

    popover.onDidDismiss().then(async ({data}: OverlayEventDetail) => {
      if (data) {
        await this.deleteDeck();
      }
    });

    await popover.present();
  }

  private deleteDeck(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.deck || !this.deck.id || this.deck.id === undefined || this.deck.id === '') {
        resolve();
        return;
      }

      this.actionInProgress = true;

      const loading: HTMLIonLoadingElement = await loadingController.create({});

      await loading.present();

      try {
        await deleteDeck(this.deck.id);

        this.deckDeleted.emit(this.deck.id);
      } catch (err) {
        errorStore.state.error = err;
      }

      await loading.dismiss();

      this.actionInProgress = false;

      resolve();
    });
  }

  private async cloneDeck($event: UIEvent): Promise<void> {
    $event.stopPropagation();

    if (this.actionInProgress) {
      return;
    }

    this.actionInProgress = true;

    const loading: HTMLIonLoadingElement = await loadingController.create({});

    await loading.present();

    try {
      await clone(this.deck);

      this.deckCloned.emit();
    } catch (err) {
      errorStore.state.error = err;
    }

    await loading.dismiss();

    this.actionInProgress = false;
  }

  render() {
    return (
      <Host>
        <button onClick={($event: UIEvent) => this.cloneDeck($event)} title={i18n.state.dashboard.copy} disabled={this.actionInProgress}>
          <AppIcon name="copy" ariaLabel="" ariaHidden={true}></AppIcon>
        </button>

        <button
          onClick={($event: UIEvent) => this.presentConfirmDelete($event)}
          title={i18n.state.dashboard.delete}
          disabled={this.actionInProgress || this.disableDelete}>
          <AppIcon name="trash" ariaLabel="" ariaHidden={true}></AppIcon>
        </button>
      </Host>
    );
  }
}
