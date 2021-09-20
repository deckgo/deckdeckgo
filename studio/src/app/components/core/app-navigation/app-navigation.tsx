import {Component, Prop, h, Host, State} from '@stencil/core';

import {isIOS} from '@deckdeckgo/utils';

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

  @Prop() actions: boolean = true;

  @State()
  private hideICP: boolean = false;

  private async closeMenu() {
    const element: HTMLIonMenuElement | null = document.querySelector('ion-menu');
    await element?.close();
  }

  render() {
    return (
      <Host>
        {this.renderICP()}
        <ion-header>
          <ion-toolbar>
            {this.renderTitle()}
            {this.renderMenuToggle()}

            {this.actions ? <app-navigation-actions slot="end"></app-navigation-actions> : undefined}
          </ion-toolbar>
        </ion-header>
      </Host>
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
