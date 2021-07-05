import {Component, Prop, h, Fragment} from '@stencil/core';

import {popoverController} from '@ionic/core';

import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';
import i18n from '../../../stores/i18n.store';

import {signIn} from '../../../utils/core/signin.utils';
import { SaveService } from '../../../services/editor/save/save.service';
import errorStore from '../../../stores/error.store';

@Component({
  tag: 'app-navigation-actions',
  styleUrl: 'app-navigation-actions.scss',
  shadow: false
})
export class AppNavigationActions {
  @Prop() signIn: boolean = true;

  private async openMenu($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-user-menu',
      event: $event,
      mode: 'ios'
    });

    await popover.present();
  }

  private async backupOfflineData() {
    try {
      await SaveService.getInstance().backup();
    } catch (err) {
      errorStore.state.error = `Something went wrong. ${err}.`;
    }
  }

  render() {
    return (
      <div>
        {this.renderActions()}
        {this.renderSignIn()}
        {this.renderLoggedIn()}
      </div>
    );
  }

  private renderActions() {
    return (
      <Fragment>
        <button class="ion-margin-start ion-margin-end ion-activatable" onClick={() => this.backupOfflineData()} tabindex={0}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-icon aria-hidden="true" src="/assets/icons/ionicons/download.svg"></ion-icon>
          <ion-label>{i18n.state.editor.export}</ion-label>
        </button>
      </Fragment>
    );
  }

  private renderSignIn() {
    if (authStore.state.loggedIn || !this.signIn) {
      return undefined;
    }

    return (
      <button class="ion-margin-end ion-activatable" onClick={() => signIn()} tabindex={0}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" name="log-in-outline"></ion-icon>
        <ion-label>{i18n.state.nav.sign_in}</ion-label>
      </button>
    );
  }

  private renderLoggedIn() {
    if (authStore.state.loggedIn && userStore.state.loaded) {
      return (
        <button class="ion-margin-end ion-activatable" onClick={(e: UIEvent) => this.openMenu(e)} aria-label={i18n.state.nav.menu} tabindex={0}>
          <ion-ripple-effect></ion-ripple-effect>
          <app-avatar src={userStore.state.photoUrl}></app-avatar>
          <ion-label>{userStore.state.name}</ion-label>
        </button>
      );
    }

    return undefined;
  }
}
