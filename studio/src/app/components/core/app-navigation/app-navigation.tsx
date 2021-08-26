import {Component, Prop, h} from '@stencil/core';

import offlineStore from '../../../stores/offline.store';
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

  private renderLogo() {
    return (
      <div class="logo">
        <app-logo></app-logo>
        <ion-label>DeckDeckGo</ion-label>
      </div>
    );
  }

  private async closeMenu() {
    if (!document) {
      return;
    }

    const element: HTMLIonMenuElement | null = document.querySelector('ion-menu');
    await element?.close();
  }

  private renderMenuToggle() {
    if (!this.menuToggle) {
      return undefined;
    }

    return (
      <ion-buttons slot="start">
        <ion-menu-toggle>
          <ion-button aria-label={i18n.state.nav.menu}>
            <AppIcon name="menu" ariaHidden={true} ariaLabel="" slot="icon-only"></AppIcon>
          </ion-button>
        </ion-menu-toggle>
      </ion-buttons>
    );
  }

  private renderUser() {
    if (!offlineStore.state.online || !this.user) {
      return undefined;
    }

    return <app-navigation-actions signIn={this.signIn} slot="end"></app-navigation-actions>;
  }
}
