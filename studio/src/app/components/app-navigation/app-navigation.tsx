import {Component, Prop} from '@stencil/core';

@Component({
  tag: 'app-navigation',
  styleUrl: 'app-navigation.scss',
  shadow: false
})
export class AppNavigation {

  @Prop({connect: 'ion-menu-controller'}) lazyMenuController!: HTMLIonMenuControllerElement;

  @Prop() logo: boolean = false;
  @Prop() menuToggle: boolean = true;

  render() {
    return (<ion-header>
          <ion-toolbar>
            {this.renderLogo()}
            {this.renderMenuToggle()}
          </ion-toolbar>
        </ion-header>
    );
  }

  private renderLogo() {
    if (this.logo) {
      return <ion-title slot="start">
        <a href="/">
          <app-logo></app-logo>
          <span>DeckDeckGo</span>
        </a>
      </ion-title>;
    } else {
      return null;
    }
  }

  private renderMenuToggle() {
    if (this.menuToggle) {
      return <ion-buttons slot="start">
        <ion-menu-toggle>
          <ion-button>
            <ion-icon slot="icon-only" name="menu"></ion-icon>
          </ion-button>
        </ion-menu-toggle>
      </ion-buttons>;
    } else {
      return null;
    }
  }

}
