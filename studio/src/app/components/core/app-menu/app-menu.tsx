import {Component, Element, State, h} from '@stencil/core';

import {Subscription} from 'rxjs';

import {AuthUser} from '../../../models/auth/auth.user';

import {Utils} from '../../../utils/core/utils';

import {AuthService} from '../../../services/auth/auth.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';

@Component({
  tag: 'app-menu',
  styleUrl: 'app-menu.scss',
  shadow: false
})
export class AppMenu {
  @Element() el: HTMLElement;

  private authService: AuthService;
  private authSubscription: Subscription;

  private navService: NavService;

  @State()
  private authUser: AuthUser;

  constructor() {
    this.authService = AuthService.getInstance();
    this.navService = NavService.getInstance();
  }

  componentWillLoad() {
    this.authSubscription = this.authService.watch().subscribe(async (authUser: AuthUser) => {
      this.authUser = authUser;
    });
  }

  componentDidUnload() {
    if (this.authSubscription) {
      this.authSubscription.unsubscribe();
    }
  }

  private async signIn() {
    this.navService.navigate({
      url: '/signin' + (window && window.location ? window.location.pathname : ''),
      direction: NavDirection.FORWARD
    });
  }

  private async signOut() {
    await this.authService.signOut();

    this.navService.navigate({
      url: '/',
      direction: NavDirection.ROOT
    });
  }

  render() {
    return (
      <ion-list>
        {this.renderUser()}

        {this.renderHome()}
        {this.renderDiscover()}
        {this.renderDashboard()}
        {this.renderSettings()}
        {this.renderSignInOut()}
      </ion-list>
    );
  }

  private renderUser() {
    if (Utils.isLoggedIn(this.authUser)) {
      return (
        <ion-item class="user">
          <app-user-info avatarColSize={3}></app-user-info>
        </ion-item>
      );
    } else {
      return <ion-item class="user"></ion-item>;
    }
  }

  private renderDashboard() {
    if (Utils.isLoggedIn(this.authUser)) {
      return (
        <ion-item button class="home" href="/dashboard" routerDirection="forward">
          <ion-icon lazy={true} name="apps" slot="start"></ion-icon>
          <ion-label>Dashboard</ion-label>
        </ion-item>
      );
    } else {
      return undefined;
    }
  }

  private renderSignInOut() {
    if (Utils.isLoggedIn(this.authUser)) {
      return (
        <ion-item button class="signout" onClick={() => this.signOut()}>
          <ion-icon lazy={true} name="log-out" slot="start"></ion-icon>
          <ion-label>Sign out</ion-label>
        </ion-item>
      );
    } else {
      return (
        <ion-item button onClick={() => this.signIn()}>
          <ion-icon lazy={true} name="log-in" slot="start"></ion-icon>
          <ion-label>Sign in</ion-label>
        </ion-item>
      );
    }
  }

  private renderHome() {
    return (
      <ion-item button class="home" href="/" routerDirection="forward">
        <ion-icon lazy={true} name="home" slot="start"></ion-icon>
        <ion-label>Home</ion-label>
      </ion-item>
    );
  }

  private renderDiscover() {
    if (Utils.isLoggedIn(this.authUser)) {
      return undefined;
    }

    return (
      <ion-item button class="home" href="/discover" routerDirection="forward">
        <ion-icon lazy={true} name="search" slot="start"></ion-icon>
        <ion-label>Discover</ion-label>
      </ion-item>
    );
  }

  private renderSettings() {
    return (
      <ion-item button class="home" href="/settings" routerDirection="forward">
        <ion-icon lazy={true} name="settings" slot="start"></ion-icon>
        <ion-label>Settings</ion-label>
      </ion-item>
    );
  }
}
