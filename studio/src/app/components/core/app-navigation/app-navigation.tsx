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

  @Prop() presentation: boolean = false;
  @Prop() publish: boolean = false;

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
      return <ion-anchor href="/" routerDirection="forward" slot="start">
        <div>
          <app-logo></app-logo>
          <span text-uppercase>DeckDeckGo</span>
        </div>
      </ion-anchor>;
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
        <app-navigation-actions presentation={this.presentation} publish={this.publish}></app-navigation-actions>
      </div>;
    } else {
      return null;
    }
  }

}
