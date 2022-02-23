import {clearEdit} from '@deckdeckgo/studio';
import {Component, Element, Fragment, h} from '@stencil/core';
import {signOut} from '../../../../providers/auth/auth.provider';
import authStore from '../../../../stores/auth.store';
import i18n from '../../../../stores/i18n.store';
import navStore, {NavDirection} from '../../../../stores/nav.store';
import syncStore from '../../../../stores/sync.store';
import userStore from '../../../../stores/user.store';

@Component({
  tag: 'app-user-menu',
  styleUrl: 'app-user-menu.scss'
})
export class AppUserMenu {
  @Element() el: HTMLElement;

  private async signUserOut() {
    await signOut();

    await clearEdit(true);

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
    return (
      <Fragment>
        {this.renderUserInfo()}
        {this.renderActions()}
      </Fragment>
    );
  }

  private renderUserInfo() {
    if (!authStore.state.loggedIn) {
      return undefined;
    }

    if (!userStore.state.name) {
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
          <ion-router-link href="/profile" routerDirection="forward">
            <ion-label>{i18n.state.nav.profile}</ion-label>
          </ion-router-link>
        </ion-item>

        <ion-item onClick={() => this.closePopover()}>
          <ion-router-link href="/templates" routerDirection="forward">
            <ion-label>{i18n.state.nav.templates}</ion-label>
          </ion-router-link>
        </ion-item>

        <ion-item onClick={() => this.closePopover()}>
          <ion-router-link href="/customization" routerDirection="forward">
            <ion-label>{i18n.state.nav.customization}</ion-label>
          </ion-router-link>
        </ion-item>

        <ion-item onClick={() => this.signUserOut()} disabled={syncStore.state.dirty || !authStore.state.loggedIn}>
          <ion-label>{i18n.state.nav.sign_out}</ion-label>
        </ion-item>
      </ion-list>
    );
  }
}
