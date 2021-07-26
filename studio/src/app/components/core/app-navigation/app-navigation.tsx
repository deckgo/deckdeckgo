import {Component, Prop, h, Host, State} from '@stencil/core';

import {isIOS} from '@deckdeckgo/utils';

import offlineStore from '../../../stores/offline.store';
import busyStore from '../../../stores/busy.store';
import i18n from '../../../stores/i18n.store';
import store from '../../../stores/deck.store';

import {AppIcon} from '../app-icon/app-icon';

@Component({
  tag: 'app-navigation',
  styleUrl: 'app-navigation.scss',
  shadow: false
})
export class AppNavigation {
  @Prop() menuToggle: boolean = true;

  @Prop() user: boolean = true;

  @Prop() signIn: boolean = false;

  @State()
  private hideICP: boolean = false;

  render() {
    return (
      <Host>
        {this.renderICP()}
        <ion-header class={offlineStore.state.offline ? 'offline' : undefined}>
          <ion-toolbar>
            {this.renderTitleOnline()}
            {this.renderTitleOffline()}
            {this.renderMenuToggle()}
            {this.renderUser()}
            {this.renderInfoOffline()}
          </ion-toolbar>
        </ion-header>
      </Host>
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
            <ion-button aria-label={i18n.state.nav.menu}>
              <AppIcon name="menu" ariaHidden={true} ariaLabel="" slot="icon-only"></AppIcon>
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

    if (this.user && busyStore.state.deckReady) {
      return <app-navigation-actions signIn={this.signIn} slot="end"></app-navigation-actions>;
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
        <ion-label>{i18n.state.offline.editing}</ion-label>
      </ion-router-link>
    );
  }

  private renderICP() {
    if (this.hideICP) {
      return undefined;
    }

    return (
      <header class={`icp ${isIOS() ? 'ios' : 'md'}`}>
        <ion-button fill="clear" color="light" onClick={() => (this.hideICP = true)}>
          <ion-icon name="close"></ion-icon>
        </ion-button>

        <p>
          DeckDeckGo Internet Computer{' '}
          <a
            href="https://dev.to/daviddalbusco/we-received-a-grant-to-port-our-web-app-to-the-internet-computer-318o"
            rel="noopener norefferer"
            target="_blank"
            aria-label="More information">
            <ion-icon name="information-circle"></ion-icon>
          </a>
        </p>
      </header>
    );
  }
}
