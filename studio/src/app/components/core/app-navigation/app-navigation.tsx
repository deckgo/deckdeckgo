import {Component, Prop, h, State} from '@stencil/core';

import {Subscription} from 'rxjs';

import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';
import {Deck} from '../../../models/data/deck';

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

  @State()
  private deckName: string = null;

  constructor() {
    this.deckEditorService = DeckEditorService.getInstance();
  }

  componentWillLoad() {
    this.subscription = this.deckEditorService.watch().subscribe((deck: Deck) => {
      this.deckName = deck && deck.data && deck.data.name && deck.data.name !== '' ? deck.data.name : null;
    });
  }

  componentDidUnload() {
    if (this.subscription) {
      this.subscription.unsubscribe();
    }
  }

  render() {
    return (
      <ion-header>
        <ion-toolbar>
          {this.renderTitle()}
          {this.renderMenuToggle()}
          {this.renderUser()}
        </ion-toolbar>
      </ion-header>
    );
  }

  private renderTitle() {
    const titleClass = this.deckName && this.deckName !== '' ? 'title deck-name-visible' : 'title';

    return (
      <div class={titleClass}>
        <ion-router-link onClick={() => this.closeMenu()} href="/" routerDirection="forward" class="home">
          <div>
            <app-logo></app-logo>
            <ion-label class="ion-text-uppercase">
              DeckDeckGo <mark>BETA</mark>
            </ion-label>
          </div>
        </ion-router-link>

        <ion-label class="ion-text-uppercase deck-name">{this.deckName}</ion-label>
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
      return null;
    }
  }

  private renderUser() {
    if (this.user) {
      return (
        <div slot="end">
          <app-navigation-actions presentation={this.presentation} publish={this.publish}></app-navigation-actions>
        </div>
      );
    } else {
      return null;
    }
  }
}
