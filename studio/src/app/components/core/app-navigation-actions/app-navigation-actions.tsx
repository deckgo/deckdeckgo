import { Component, Prop, h, Fragment, Element } from '@stencil/core';

import { alertController, popoverController } from '@ionic/core';

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
  @Element() el: HTMLElement;

  @Prop() signIn: boolean = true;

  private async openMenu($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-user-menu',
      event: $event,
      mode: 'ios'
    });

    await popover.present();
  }

  private async exportData() {
    try {
      await SaveService.getInstance().backup();
    } catch (err) {
      errorStore.state.error = `Something went wrong. ${err}.`;
    }
  }

  private async newDeck() {
    const alert: HTMLIonAlertElement = await alertController.create({
      header: i18n.state.tools.new_presentation,
      message: i18n.state.tools.new_warning_text,
      buttons: [
        i18n.state.core.cancel,
        { text: i18n.state.core.ok, handler: () => this.emitInitNewDeck() }
      ]
    });

    await alert.present();
  }

  private async emitInitNewDeck() {
    const initNewDeck: CustomEvent<void> = new CustomEvent<void>('initNewDeck', {
      bubbles: true
    });

    this.el.dispatchEvent(initNewDeck);
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
        <button class="ion-margin-end ion-activatable" onClick={() => this.newDeck()} tabindex={0} aria-label={i18n.state.tools.new_presentation}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-icon aria-hidden="true" src="/assets/icons/ionicons/document.svg"></ion-icon>
          <ion-label>{i18n.state.tools.new}</ion-label>
        </button>

        <button class="ion-margin-end ion-activatable" onClick={() => this.exportData()} tabindex={0}>
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
