import {Component, Prop, h, Host, State} from '@stencil/core';

import {isIOS} from '@deckdeckgo/utils';

import i18n from '../../../stores/i18n.store';
import editorStore from '../../../stores/editor.store';

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
  private hideIC: boolean = localStorage.getItem('deckgo-hide-announcement-ic') !== null;

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
    const titleClass = editorStore.state.name && editorStore.state.name !== '' ? 'title deck-name-visible' : 'title';

    return (
      <div class={titleClass}>
        <ion-router-link onClick={() => this.closeMenu()} href="/" routerDirection="forward" class="nav">
          {this.renderLogo()}
        </ion-router-link>

        <ion-label class="display-name">{editorStore.state.name}</ion-label>
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
    if (this.hideIC) {
      return undefined;
    }

    return (
      <header class={`ic ${isIOS() ? 'ios' : 'md'}`}>
        <ion-button
          fill="clear"
          color="light"
          onClick={() => {
            this.hideIC = true;
            localStorage.setItem('deckgo-hide-announcement-ic', 'true');
          }}>
          <AppIcon name="close" ariaLabel={i18n.state.core.close}></AppIcon>
        </ion-button>

        <p>
          <a
            href="https://medium.com/geekculture/bye-bye-amazon-google-hello-web-3-0-b01bfe8f8783"
            rel="noopener norefferer"
            target="_blank">
            We are porting DeckDeckGo to DFINITY’s Internet Computer.
          </a>
        </p>
      </header>
    );
  }
}
