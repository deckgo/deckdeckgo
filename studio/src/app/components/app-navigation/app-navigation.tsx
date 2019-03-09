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
  @Prop() user: boolean = true;

  render() {
    return (<ion-header>
          <ion-toolbar>
            {this.renderLogo()}
            {this.renderMenuToggle()}
            {this.renderUser()}
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

  private renderUser() {
    if (this.user) {
      return <div slot="end">
        <ion-button color="primary" shape="round">
          <ion-icon name="add" slot="start"></ion-icon>
          <ion-label text-uppercase>Write a presentation</ion-label>
        </ion-button>

        <a padding-start padding-end>
          <app-avatar></app-avatar>
        </a>
      </div>;
    } else {
      return null;
    }
  }

}
