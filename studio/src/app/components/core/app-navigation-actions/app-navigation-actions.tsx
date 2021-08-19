import {Component, Prop, h} from '@stencil/core';

import {popoverController} from '@ionic/core';

import themeStore from '../../../stores/theme.store';
import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';
import i18n from '../../../stores/i18n.store';

import {signIn} from '../../../utils/core/signin.utils';

@Component({
  tag: 'app-navigation-actions',
  styleUrl: 'app-navigation-actions.scss',
  shadow: false
})
export class AppNavigationActions {
  @Prop() signIn: boolean = true;
  @Prop() write: boolean = true;
  @Prop() publish: boolean = false;

  private async openMenu($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-user-menu',
      event: $event,
      mode: 'ios'
    });

    await popover.present();
  }

  render() {
    return (
      <div>
        {this.renderSignIn()}
        {this.renderPresentationButton()}
        {this.renderLoggedIn()}
      </div>
    );
  }

  private renderSignIn() {
    if (authStore.state.loggedIn || !this.signIn) {
      return undefined;
    } else if (this.publish) {
      return (
        <button class="wide-device ion-padding-start ion-padding-end signin" onClick={() => signIn()} tabindex={0}>
          <ion-label>{i18n.state.nav.sign_in}</ion-label>
        </button>
      );
    }
  }

  private renderLoggedIn() {
    if (authStore.state.loggedIn && userStore.state.loaded) {
      return (
        <button class="ion-padding-end" onClick={(e: UIEvent) => this.openMenu(e)} aria-label={i18n.state.nav.menu} tabindex={0}>
          <app-avatar src={userStore.state.photoUrl}></app-avatar>
        </button>
      );
    } else {
      return undefined;
    }
  }

  private renderPresentationButton() {
    if (this.write && !this.publish) {
      return (
        <app-start-deck writeColor={themeStore.state.darkTheme ? 'light' : 'dark'} importColor={themeStore.state.darkTheme ? 'light' : 'dark'}></app-start-deck>
      );
    } else {
      return null;
    }
  }
}
