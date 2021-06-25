import {Component, Element, Fragment, h} from '@stencil/core';

import navStore from '../../../stores/nav.store';
import authStore from '../../../stores/auth.store';
import {NavDirection} from '../../../stores/nav.store';
import i18n from '../../../stores/i18n.store';

import {signIn} from '../../../utils/core/signin.utils';

import {AuthService} from '../../../services/auth/auth.service';

@Component({
  tag: 'app-menu',
  styleUrl: 'app-menu.scss',
  shadow: false
})
export class AppMenu {
  @Element() el: HTMLElement;

  private authService: AuthService;

  constructor() {
    this.authService = AuthService.getInstance();
  }

  private async signIn() {
    signIn();
  }

  private async signOut() {
    await this.authService.signOut();

    navStore.state.nav = {
      url: '/',
      direction: NavDirection.RELOAD
    };
  }

  render() {
    return (
      <ion-list>
        {this.renderUser()}

        {this.renderDashboard()}
        {this.renderSettings()}

        {this.renderInteract()}

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
    return (
      <ion-item button class="home" href="/dashboard" routerDirection="forward">
        <ion-icon lazy={true} name="apps-outline" slot="start"></ion-icon>
        <ion-label>{i18n.state.menu.dashboard}</ion-label>
      </ion-item>
    );
  }

  private renderSignInOut() {
    if (authStore.state.loggedIn) {
      return (
        <ion-item button class="signout" onClick={() => this.signOut()}>
          <ion-icon lazy={true} name="log-out-outline" slot="start" style={{transform: 'translate(3px, 0px)'}}></ion-icon>
          <ion-label>{i18n.state.nav.sign_out}</ion-label>
        </ion-item>
      );
    } else {
      return (
        <ion-item button onClick={() => this.signIn()}>
          <ion-icon lazy={true} name="log-in-outline" slot="start" style={{transform: 'translate(-3px, 0px)'}}></ion-icon>
          <ion-label>{i18n.state.nav.sign_in}</ion-label>
        </ion-item>
      );
    }
  }

  private renderInteract() {
    return (
      <Fragment>
        <ion-item button class="home" href="/poll" routerDirection="forward">
          <ion-icon lazy={true} name="chatbubble-ellipses-outline" slot="start"></ion-icon>
          <ion-label>{i18n.state.menu.poll}</ion-label>
        </ion-item>

        <ion-item button class="home remote" href="https://deckdeckgo.app" target="_blank">
          <ion-icon lazy={true} name="phone-portrait-outline" slot="start"></ion-icon>
          <ion-label>{i18n.state.menu.remote_control}</ion-label>
        </ion-item>
      </Fragment>
    );
  }

  private renderSettings() {
    return (
      <app-expansion-panel expanded="close">
        <ion-label slot="title">Settings</ion-label>
        <ion-icon lazy={true} name="settings-outline" slot="icon"></ion-icon>

        <ion-list class="settings">
          <ion-item button class="home" href="/profile" routerDirection="forward">
            <ion-label>{i18n.state.nav.profile}</ion-label>
            <ion-icon lazy={true} name="person-outline" slot="start"></ion-icon>
          </ion-item>
          <ion-item button class="home" href="/customization" routerDirection="forward">
            <ion-label>{i18n.state.nav.customization}</ion-label>
            <ion-icon lazy={true} name="color-palette-outline" slot="start"></ion-icon>
          </ion-item>
          <ion-item button class="home" href="/templates" routerDirection="forward">
            <ion-label>{i18n.state.nav.templates}</ion-label>
            <ion-icon lazy={true} name="reader-outline" slot="start"></ion-icon>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
