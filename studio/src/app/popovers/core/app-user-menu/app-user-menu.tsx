import {Component, Element, Fragment, h} from '@stencil/core';

import navStore, {NavDirection} from '../../../stores/nav.store';
import i18n from '../../../stores/i18n.store';
import apiUserStore from '../../../stores/api.user.store';
import userStore from '../../../stores/user.store';
import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';

import {AuthService} from '../../../providers/auth/auth.service';
import {AuthFactoryService} from '../../../providers/auth/auth.factory.service';

@Component({
  tag: 'app-user-menu',
  styleUrl: 'app-user-menu.scss'
})
export class AppUserMenu {
  @Element() el: HTMLElement;

  private readonly authService: AuthService;

  constructor() {
    this.authService = AuthFactoryService.getInstance();
  }

  private async signOut() {
    await this.authService.signOut();

    await this.closePopover();

    navStore.state.nav = {
      url: '/',
      direction: NavDirection.RELOAD
    };
  }

  async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  render() {
    return [this.renderUserInfo(), this.renderActions()];
  }

  private renderUserInfo() {
    if (authStore.state.anonymous) {
      return undefined;
    }

    const username: string | undefined = apiUserStore.state.apiUser?.username || userStore.state.user?.data?.username;

    if (!userStore.state.name && !username) {
      return undefined;
    }

    return (
      <Fragment>
        <app-user-info></app-user-info>

        <hr />
      </Fragment>
    );
  }

  private renderActions() {
    return (
      <ion-list>
        <ion-item onClick={() => this.closePopover()}>
          <ion-router-link href="/dashboard" routerDirection="forward">
            <ion-label>{i18n.state.nav.dashboard}</ion-label>
          </ion-router-link>
        </ion-item>

        <ion-item onClick={() => this.closePopover()}>
          <ion-router-link href="/customization" routerDirection="forward">
            <ion-label>{i18n.state.nav.customization}</ion-label>
          </ion-router-link>
        </ion-item>

        <ion-item onClick={() => this.closePopover()}>
          <ion-router-link href="/templates" routerDirection="forward">
            <ion-label>{i18n.state.nav.templates}</ion-label>
          </ion-router-link>
        </ion-item>

        <ion-item onClick={() => this.closePopover()}>
          <ion-router-link href="/profile" routerDirection="forward">
            <ion-label>{i18n.state.nav.profile}</ion-label>
          </ion-router-link>
        </ion-item>

        <ion-item onClick={() => this.signOut()} disabled={['pending', 'in_progress'].includes(syncStore.state.sync)}>
          <ion-label>{i18n.state.nav.sign_out}</ion-label>
        </ion-item>
      </ion-list>
    );
  }
}
