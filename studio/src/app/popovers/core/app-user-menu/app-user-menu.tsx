import {Component, Element, h} from '@stencil/core';

import navStore, {NavDirection} from '../../../stores/nav.store';
import i18n from '../../../stores/i18n.store';

import {AuthService} from '../../../services/auth/auth.service';
import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';

@Component({
  tag: 'app-user-menu',
  styleUrl: 'app-user-menu.scss',
})
export class AppUserMenu {
  @Element() el: HTMLElement;

  private authService: AuthService;

  private imageHistoryService: ImageHistoryService;

  constructor() {
    this.authService = AuthService.getInstance();
    this.imageHistoryService = ImageHistoryService.getInstance();
  }

  private async signOut() {
    await this.authService.signOut();
    await this.imageHistoryService.clear();

    await this.closePopover();

    navStore.state.nav = {
      url: '/',
      direction: NavDirection.RELOAD,
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
          <ion-router-link href="/profile" routerDirection="forward">
            <ion-label>{i18n.state.nav.profile}</ion-label>
          </ion-router-link>
        </ion-item>

        <ion-item onClick={() => this.signOut()}>
          <ion-label>{i18n.state.nav.sign_out}</ion-label>
        </ion-item>
      </ion-list>
    );
  }
}
