import {Component, Event, EventEmitter, Prop, h} from '@stencil/core';

import {popoverController} from '@ionic/core';

import themeStore from '../../../stores/theme.store';
import navStore, {NavDirection} from '../../../stores/nav.store';
import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';

@Component({
  tag: 'app-navigation-actions',
  styleUrl: 'app-navigation-actions.scss',
  shadow: false,
})
export class AppNavigationActions {
  @Prop() signIn: boolean = true;
  @Prop() presentation: boolean = false;
  @Prop() publish: boolean = false;

  @Event() private actionPublish: EventEmitter<void>;

  private async openMenu($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-user-menu',
      event: $event,
      mode: 'ios',
    });

    await popover.present();
  }

  private async navigateSignIn() {
    navStore.state.nav = {
      url: '/signin' + (window.location?.pathname ?? ''),
      direction: NavDirection.FORWARD,
    };
  }

  render() {
    return (
      <div>
        {this.renderSignIn()}
        {this.renderPresentationButton()}
        {this.renderPublishButton()}
        {this.renderLoggedIn()}
      </div>
    );
  }

  private renderSignIn() {
    if (authStore.state.loggedIn || !this.signIn) {
      return undefined;
    } else if (this.presentation || this.publish) {
      return (
        <button class="wide-device ion-padding-start ion-padding-end signin" onClick={() => this.navigateSignIn()} tabindex={0}>
          <ion-label>Sign in</ion-label>
        </button>
      );
    }
  }

  private renderLoggedIn() {
    if (authStore.state.loggedIn && userStore.state.loaded) {
      return (
        <button class="ion-padding-end" onClick={(e: UIEvent) => this.openMenu(e)} aria-label="Open menu" tabindex={0}>
          <app-avatar src={userStore.state.photoUrl}></app-avatar>
        </button>
      );
    } else {
      return undefined;
    }
  }

  private renderPresentationButton() {
    if (this.presentation) {
      return (
        <ion-button
          class="presentation ion-margin-end"
          shape="round"
          href="/editor"
          routerDirection="root"
          mode="md"
          color={themeStore.state.darkTheme ? 'light' : 'dark'}>
          <ion-label>Write a presentation</ion-label>
        </ion-button>
      );
    } else {
      return null;
    }
  }

  private renderPublishButton() {
    if (this.publish) {
      return (
        <ion-button
          class="publish ion-margin-end"
          shape="round"
          onClick={() => this.actionPublish.emit()}
          mode="md"
          color={themeStore.state.darkTheme ? 'light' : 'dark'}>
          <ion-label>Ready to share?</ion-label>
        </ion-button>
      );
    } else {
      return null;
    }
  }
}
