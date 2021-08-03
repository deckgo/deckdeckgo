import {Component, Event, EventEmitter, h, Prop, Host, State} from '@stencil/core';
import type {OverlayEventDetail} from '@ionic/core';

import store from '../../../stores/error.store';
import i18n from '../../../stores/i18n.store';

import {Deck} from '../../../models/data/deck';

import {DeckDashboardCloneResult, DeckDashboardService} from '../../../services/deck/deck-dashboard.service';
import {DeckService, initDeckService} from '../../../services/data/deck/deck.service';

import {AppIcon} from '../../core/app-icon/app-icon';

import {loadingController, modalController} from '../../../utils/ionic/ionic.overlay';

@Component({
  tag: 'app-dashboard-deck-actions',
  styleUrl: 'app-dashboard-deck-actions.scss',
  shadow: true
})
export class AppDashboardDeckActions {
  @Prop() deck: Deck;

  @Prop() cloud: 'offline' | 'firebase' | 'ic';

  private deckService: DeckService;
  private deckDashboardService: DeckDashboardService;

  @Event() deckDeleted: EventEmitter<string>;
  @Event() deckCloned: EventEmitter<DeckDashboardCloneResult>;

  @State()
  private actionInProgress: boolean = false;

  constructor() {
    this.deckService = initDeckService();
    this.deckDashboardService = DeckDashboardService.getInstance();
  }

  private async presentConfirmDelete($event: UIEvent) {
    $event.stopPropagation();

    if (this.actionInProgress) {
      return;
    }

    const disabled: boolean = this.deck?.data?.clone !== undefined;

    if (disabled) {
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
        await this.deckService.delete(this.deck.id);

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
    return new Promise<void>(async (resolve) => {
      $event.stopPropagation();

      if (this.actionInProgress || this.cloud !== 'firebase') {
        resolve();
        return;
      }

      const disabled: boolean = this.deck?.data?.clone !== undefined;

      if (disabled) {
        resolve();
        return;
      }

      if (!this.deck || !this.deck.id || this.deck.id === undefined || this.deck.id === '') {
        resolve();
        return;
      }

      if (!this.deck.data) {
        resolve();
        return;
      }

      this.actionInProgress = true;

      const loading: HTMLIonLoadingElement = await loadingController.create({});

      await loading.present();

      try {
        const clone: DeckDashboardCloneResult = await this.deckDashboardService.clone(this.deck);

        this.deckCloned.emit(clone);
      } catch (err) {
        store.state.error = err;
      }

      await loading.dismiss();

      this.actionInProgress = false;

      resolve();
    });
  }

  render() {
    const disabled: boolean = this.deck && this.deck.data && this.deck.data.clone !== undefined;

    return (
      <Host>
        {this.renderClone(disabled)}

        <button
          onClick={($event: UIEvent) => this.presentConfirmDelete($event)}
          title={i18n.state.dashboard.delete}
          class={this.actionInProgress || disabled ? 'disabled' : undefined}>
          <AppIcon name="trash" ariaLabel="" ariaHidden={true}></AppIcon>
        </button>
      </Host>
    );
  }

  private renderClone(disabled: boolean) {
    if (this.cloud !== 'firebase') {
      return undefined;
    }

    return (
      <button
        onClick={($event: UIEvent) => this.cloneDeck($event)}
        title={i18n.state.dashboard.copy}
        class={this.actionInProgress || disabled ? 'disabled' : undefined}>
        <AppIcon name="copy" ariaLabel="" ariaHidden={true}></AppIcon>
      </button>
    );
  }
}
