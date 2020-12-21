import {Component, Prop, h} from '@stencil/core';

import offlineStore from '../../../stores/offline.store';

import store from '../../../stores/deck.store';

@Component({
  tag: 'app-navigation',
  styleUrl: 'app-navigation.scss',
  shadow: false,
})
export class AppNavigation {
  @Prop() menuToggle: boolean = true;

  @Prop() user: boolean = true;

  @Prop() presentation: boolean = false;
  @Prop() publish: boolean = false;

  render() {
    return (
      <ion-header class={offlineStore.state.offline ? 'offline' : undefined}>
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
    if (offlineStore.state.offline !== undefined) {
      return undefined;
    }

    const titleClass = store.state.name && store.state.name !== '' ? 'title deck-name-visible' : 'title';

    return (
      <div class={titleClass}>
        <ion-router-link onClick={() => this.closeMenu()} href="/" routerDirection="forward" class="nav">
          {this.renderLogo()}
        </ion-router-link>

        <ion-label class="deck-name">{store.state.name}</ion-label>
      </div>
    );
  }

  private renderTitleOffline() {
    if (offlineStore.state.offline === undefined) {
      return undefined;
    }

    return (
      <div class="title offline deck-name-visible">
        <ion-router-link href={`/editor/${offlineStore.state.offline.id}`} routerDirection="root" class="home">
          <div>
            {this.renderLogo()}

            <ion-label>{offlineStore.state.offline.name}</ion-label>
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
    if (offlineStore.state.offline !== undefined) {
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
    if (offlineStore.state.offline !== undefined) {
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
    if (offlineStore.state.offline === undefined) {
      return undefined;
    }

    return (
      <ion-router-link href={`/editor/${offlineStore.state.offline.id}`} routerDirection="root" slot="end" class="offline-info ion-padding-end">
        <ion-label>You are editing offline.</ion-label>
      </ion-router-link>
    );
  }
}
