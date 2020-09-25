import {Component, Prop, h} from '@stencil/core';

import {modalController} from '@ionic/core';

@Component({
  tag: 'app-navigation',
  styleUrl: 'app-navigation.scss',
  shadow: false,
})
export class AppNavigation {
  @Prop() logo: boolean = false;
  @Prop() menuToggle: boolean = true;
  @Prop() navigation: boolean = true;

  render() {
    return (
      <ion-header>
        <ion-toolbar>
          {this.renderLogo()}
          {this.renderMenuToggle()}
          {this.renderNavigation()}
        </ion-toolbar>
      </ion-header>
    );
  }

  private renderLogo() {
    if (this.logo) {
      return (
        <ion-title slot="start" class="ion-no-padding ion-margin-start ion-margin-end">
          <a href="/">
            <app-logo></app-logo>
            <span>DeckDeckGo</span>
          </a>
        </ion-title>
      );
    } else {
      return null;
    }
  }

  private renderMenuToggle() {
    if (this.menuToggle) {
      return (
        <ion-buttons slot="start">
          <ion-menu-toggle>
            <ion-button color="switcher">
              <ion-icon slot="icon-only" name="menu"></ion-icon>
            </ion-button>
          </ion-menu-toggle>
        </ion-buttons>
      );
    } else {
      return null;
    }
  }

  private async openNavigationMenuModal() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-navigation-modal',
    });

    await modal.present();
  }

  private renderNavigation() {
    if (this.navigation) {
      return (
        <div slot="end">
          <ion-button onClick={() => this.openNavigationMenuModal()}>
            <ion-icon ios="ellipsis-horizontal" md="ellipsis-vertical"></ion-icon>
          </ion-button>
          <div class="links">
            <a href="https://deckdeckgo.com" class="ion-padding-start ion-padding-end">
              <app-logo></app-logo>
              <ion-label>Editor</ion-label>
            </a>
            <a href="https://github.com/deckgo" class="ion-padding-start ion-padding-end">
              <ion-icon name="logo-github" aria-lable="GitHub"></ion-icon>
              <ion-label>GitHub</ion-label>
            </a>
            <app-theme-switcher></app-theme-switcher>
          </div>
        </div>
      );
    } else {
      return null;
    }
  }
}
