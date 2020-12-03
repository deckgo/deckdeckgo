import {Component, Element, h} from '@stencil/core';

import navStore from '../../../stores/nav.store';
import authStore from '../../../stores/auth.store';

import {AuthService} from '../../../services/auth/auth.service';
import {NavDirection} from '../../../stores/nav.store';

@Component({
  tag: 'app-menu',
  styleUrl: 'app-menu.scss',
  shadow: false,
})
export class AppMenu {
  @Element() el: HTMLElement;

  private authService: AuthService;

  constructor() {
    this.authService = AuthService.getInstance();
  }

  private async signIn() {
    navStore.state.nav = {
      url: '/signin' + (window.location?.pathname ?? ''),
      direction: NavDirection.FORWARD,
    };
  }

  private async signOut() {
    await this.authService.signOut();

    navStore.state.nav = {
      url: '/',
      direction: NavDirection.RELOAD,
    };
  }

  render() {
    return (
      <ion-list>
        {this.renderUser()}

        {this.renderHome()}
        {this.renderDiscover()}
        {this.renderEnterprise()}
        {this.renderDashboard()}
        {this.renderSettings()}
        {this.renderSignInOut()}
      </ion-list>
    );
  }

  private renderUser() {
    if (authStore.state.loggedIn) {
      return (
        <ion-item class="user">
          <app-user-info displayAvatar={true}></app-user-info>
        </ion-item>
      );
    } else {
      return <ion-item class="user"></ion-item>;
    }
  }

  private renderDashboard() {
    if (authStore.state.loggedIn) {
      return (
        <ion-item button class="home" href="/dashboard" routerDirection="forward">
          <ion-icon lazy={true} name="apps-outline" slot="start"></ion-icon>
          <ion-label>Dashboard</ion-label>
        </ion-item>
      );
    } else {
      return undefined;
    }
  }

  private renderSignInOut() {
    if (authStore.state.loggedIn) {
      return (
        <ion-item button class="signout" onClick={() => this.signOut()}>
          <ion-icon lazy={true} name="log-out-outline" slot="start"></ion-icon>
          <ion-label>Sign out</ion-label>
        </ion-item>
      );
    } else {
      return (
        <ion-item button onClick={() => this.signIn()}>
          <ion-icon lazy={true} name="log-in-outline" slot="start"></ion-icon>
          <ion-label>Sign in</ion-label>
        </ion-item>
      );
    }
  }

  private renderHome() {
    return (
      <ion-item button class="home" href="/home" routerDirection="forward">
        <app-logo slot="start"></app-logo>
        <ion-label>DeckDeckGo</ion-label>
      </ion-item>
    );
  }

  private renderDiscover() {
    return (
      <ion-item button class="home" href="/discover" routerDirection="forward">
        <ion-icon lazy={true} name="search-outline" slot="start"></ion-icon>
        <ion-label>Discover</ion-label>
      </ion-item>
    );
  }

  private renderEnterprise() {
    return (
      <ion-item button class="home" href="/enterprise" routerDirection="forward">
        <ion-icon lazy={true} name="business-outline" slot="start"></ion-icon>
        <ion-label>Enterprise</ion-label>
      </ion-item>
    );
  }

  private renderSettings() {
    return (
      <app-expansion-panel expanded="close">
        <ion-label slot="title">Settings</ion-label>
        <ion-icon lazy={true} name="settings-outline" slot="icon"></ion-icon>

        <ion-list class="settings">
          <ion-item button class="home" href="/settings/profile" routerDirection="forward">
            <ion-label>Profile</ion-label>
            <ion-icon lazy={true} name="person-outline" slot="start"></ion-icon>
          </ion-item>
          <ion-item button class="home" href="/settings/customization" routerDirection="forward">
            <ion-label>Customization</ion-label>
            <ion-icon lazy={true} name="color-palette-outline" slot="start"></ion-icon>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
