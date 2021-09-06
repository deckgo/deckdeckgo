import {Component, Element, h} from '@stencil/core';

import navStore, {NavDirection} from '../../../stores/nav.store';
import i18n from '../../../stores/i18n.store';
import syncStore from '../../../stores/sync.store';

import {AuthService} from '../../../services/auth/auth.service';

@Component({
  tag: 'app-user-menu',
  styleUrl: 'app-user-menu.scss'
})
export class AppUserMenu {
  @Element() el: HTMLElement;

  private authService: AuthService;

  constructor() {
    this.authService = AuthService.getInstance();
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
    return [<app-user-info></app-user-info>, <hr />, this.renderActions()];
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
