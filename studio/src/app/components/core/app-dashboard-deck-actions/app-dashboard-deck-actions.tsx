import {Component, Event, EventEmitter, h, Prop, Host, State} from '@stencil/core';

import type {OverlayEventDetail} from '@ionic/core';

import {Deck} from '@deckdeckgo/editor';

import store from '../../../stores/error.store';
import i18n from '../../../stores/i18n.store';

import {loadingController, modalController} from '../../../utils/ionic/ionic.overlay';
import {clone} from '../../../utils/core/dashboard.utils';

import {deleteDeck} from '../../../providers/data/deck/deck.provider';

import {AppIcon} from '../../core/app-icon/app-icon';

@Component({
  tag: 'app-dashboard-deck-actions',
  styleUrl: 'app-dashboard-deck-actions.scss',
  shadow: true
})
export class AppDashboardDeckActions {
  @Prop() deck: Deck;

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

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-deck-delete',
      componentProps: {
        deckName: this.deck.data.name,
        published: this.deck.data.meta?.published
      }
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        await this.deleteDeck();
      }
    });

    await modal.present();
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
        store.state.error = err;
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
      store.state.error = err;
    }

    await loading.dismiss();

    this.actionInProgress = false;
  }

  render() {
    return (
      <Host>
        <button
          onClick={($event: UIEvent) => this.cloneDeck($event)}
          title={i18n.state.dashboard.copy}
          disabled={this.actionInProgress}
          class={this.actionInProgress ? 'disabled' : undefined}>
          <AppIcon name="copy" ariaLabel="" ariaHidden={true}></AppIcon>
        </button>

        <button
          onClick={($event: UIEvent) => this.presentConfirmDelete($event)}
          title={i18n.state.dashboard.delete}
          disabled={this.actionInProgress}
          class={this.actionInProgress ? 'disabled' : undefined}>
          <AppIcon name="trash" ariaLabel="" ariaHidden={true}></AppIcon>
        </button>
      </Host>
    );
  }
}
