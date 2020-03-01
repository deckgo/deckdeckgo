import {Component, Prop, h, State} from '@stencil/core';

import {Subscription} from 'rxjs';

import {Deck} from '../../../models/data/deck';

import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';
import {OfflineService} from '../../../services/editor/offline/offline.service';

@Component({
  tag: 'app-navigation',
  styleUrl: 'app-navigation.scss',
  shadow: false
})
export class AppNavigation {
  @Prop() menuToggle: boolean = true;

  @Prop() user: boolean = true;

  @Prop() presentation: boolean = false;
  @Prop() publish: boolean = false;

  private subscription: Subscription;
  private deckEditorService: DeckEditorService;

  private offlineSubscription: Subscription;
  private offlineService: OfflineService;

  @State()
  private deckName: string = null;

  @State()
  private offline: OfflineDeck = undefined;

  constructor() {
    this.deckEditorService = DeckEditorService.getInstance();
    this.offlineService = OfflineService.getInstance();
  }

  componentWillLoad() {
    this.subscription = this.deckEditorService.watch().subscribe((deck: Deck) => {
      this.deckName = deck && deck.data && deck.data.name && deck.data.name !== '' ? deck.data.name : null;
    });

    this.subscription = this.offlineService.watchOffline().subscribe((offline: OfflineDeck | undefined) => {
      this.offline = offline;
    });
  }

  componentDidUnload() {
    if (this.subscription) {
      this.subscription.unsubscribe();
    }

    if (this.offlineSubscription) {
      this.offlineSubscription.unsubscribe();
    }
  }

  render() {
    return (
      <ion-header class={this.offline ? 'offline' : undefined}>
        <ion-toolbar>
          {this.renderTitleOnline()}
          {this.renderTitleOffline()}
          {this.renderMenuToggle()}
          {this.renderUser()}
          {this.renderInfoOffline()}
        </ion-toolbar>
      </ion-header>
    );
  }

  private renderTitleOnline() {
    if (this.offline !== undefined) {
      return undefined;
    }

    const titleClass = this.deckName && this.deckName !== '' ? 'title deck-name-visible' : 'title';

    return (
      <div class={titleClass}>
        <ion-router-link onClick={() => this.closeMenu()} href="/" routerDirection="forward" class="home">
          {this.renderLogo()}
        </ion-router-link>

        <ion-label class="deck-name">{this.deckName}</ion-label>
      </div>
    );
  }

  private renderTitleOffline() {
    if (this.offline === undefined) {
      return undefined;
    }

    return (
      <div class="title offline deck-name-visible">
        <ion-router-link href={`/editor/${this.offline.id}`} routerDirection="root" class="home">
          <div>
            {this.renderLogo()}

            <ion-label>{this.offline.name}</ion-label>
          </div>
        </ion-router-link>
      </div>
    );
  }

  private renderLogo() {
    return (
      <div class="logo">
        <app-logo></app-logo>
        <ion-label>DeckDeckGo</ion-label>
      </div>
    );
  }

  private closeMenu(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!document) {
        return;
      }

      const element: HTMLIonMenuElement = document.querySelector('ion-menu');

      if (!element) {
        resolve();
        return;
      }

      await element.close();

      resolve();
    });
  }

  private renderMenuToggle() {
    if (this.offline !== undefined) {
      return undefined;
    }

    if (this.menuToggle) {
      return (
        <ion-buttons slot="start">
          <ion-menu-toggle>
            <ion-button>
              <ion-icon slot="icon-only" name="menu"></ion-icon>
            </ion-button>
          </ion-menu-toggle>
        </ion-buttons>
      );
    } else {
      return undefined;
    }
  }

  private renderUser() {
    if (this.offline !== undefined) {
      return undefined;
    }

    if (this.user) {
      return (
        <div slot="end">
          <app-navigation-actions presentation={this.presentation} publish={this.publish}></app-navigation-actions>
        </div>
      );
    } else {
      return undefined;
    }
  }

  private renderInfoOffline() {
    if (this.offline === undefined) {
      return undefined;
    }

    return (
      <ion-router-link href={`/editor/${this.offline.id}`} routerDirection="root" slot="end" class="offline-info ion-padding-end">
        <ion-label>You are editing offline.</ion-label>
      </ion-router-link>
    );
  }
}
