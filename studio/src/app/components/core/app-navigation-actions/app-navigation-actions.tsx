import {Component, Prop, h, Fragment, Element} from '@stencil/core';

import {alertController, loadingController, popoverController} from '@ionic/core';

import {del} from 'idb-keyval';

import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';
import i18n from '../../../stores/i18n.store';
import errorStore from '../../../stores/error.store';
import syncStore from '../../../stores/sync.store';

import {signIn} from '../../../utils/core/signin.utils';

import {FileSystemService} from '../../../services/editor/file-system/file-system.service';

@Component({
  tag: 'app-navigation-actions',
  styleUrl: 'app-navigation-actions.scss',
  shadow: false
})
export class AppNavigationActions {
  @Element() el: HTMLElement;

  @Prop() signIn: boolean = false;

  private loadInput!: HTMLInputElement;

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
      await FileSystemService.getInstance().exportData();
    } catch (err) {
      errorStore.state.error = `Something went wrong. ${err}.`;
    }
  }

  private openFilePicker() {
    this.loadInput?.click();
  }

  private async importData() {
    if (!this.loadInput || this.loadInput.files?.length <= 0) {
      return;
    }

    const loading: HTMLIonLoadingElement = await loadingController.create({});

    await loading.present();

    try {
      await FileSystemService.getInstance().importData(this.loadInput.files[0]);

      this.emitReloadDeck();
    } catch (err) {
      errorStore.state.error = `Something went wrong. ${err}.`;
    }

    await loading.dismiss();
  }

  private async newDeck() {
    const alert: HTMLIonAlertElement = await alertController.create({
      header: i18n.state.tools.new_presentation,
      message: i18n.state.tools.new_warning_text,
      buttons: [
        i18n.state.core.cancel,
        {
          text: i18n.state.core.ok,
          handler: async () => {
            // By removing the reference to the current deck in indexeddb, it will create a new deck on reload
            await del('deckdeckgo_deck_id');

            this.emitReloadDeck();
          }
        }
      ]
    });

    await alert.present();
  }

  private emitReloadDeck() {
    const initNewDeck: CustomEvent<void> = new CustomEvent<void>('reloadDeck', {
      bubbles: true
    });

    this.el.dispatchEvent(initNewDeck);
  }

  render() {
    if (this.signIn) {
      return undefined;
    }

    return (
      <Fragment>
        {this.renderActions()}
        {this.renderSignIn()}
        {this.renderLoggedInActions()}
      </Fragment>
    );
  }

  private renderActions() {
    return (
      <Fragment>
        <button
          class="ion-activatable"
          onClick={() => this.newDeck()}
          disabled={syncStore.state.sync !== 'idle'}
          aria-label={i18n.state.tools.new_presentation}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-icon aria-hidden="true" src="/assets/icons/ionicons/document.svg"></ion-icon>
          <ion-label>{i18n.state.tools.new}</ion-label>
        </button>

        <button class="ion-activatable" onClick={() => this.openFilePicker()}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-icon aria-hidden="true" src="/assets/icons/ionicons/folder-open.svg"></ion-icon>
          <ion-label>{i18n.state.tools.open}</ion-label>
        </button>

        <input type="file" accept="application/json" onChange={() => this.importData()} ref={(el) => (this.loadInput = el as HTMLInputElement)} tabindex="-1" />

        <button class="ion-activatable" onClick={() => this.exportData()}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-icon aria-hidden="true" src="/assets/icons/ionicons/download.svg"></ion-icon>
          <ion-label>{i18n.state.editor.export}</ion-label>
        </button>
      </Fragment>
    );
  }

  private renderSignIn() {
    if (authStore.state.loggedIn) {
      return undefined;
    }

    return (
      <button class="ion-activatable" onClick={() => signIn()}>
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" name="log-in-outline"></ion-icon>
        <ion-label>{i18n.state.nav.sign_in}</ion-label>
      </button>
    );
  }

  private renderLoggedInActions() {
    if (authStore.state.loggedIn && userStore.state.loaded) {
      return (
        <Fragment>
          {this.renderCloudStatus()}

          <button class="ion-activatable" onClick={(e: UIEvent) => this.openMenu(e)} aria-label={i18n.state.nav.menu}>
            <ion-ripple-effect></ion-ripple-effect>
            <app-avatar src={userStore.state.photoUrl}></app-avatar>
            <ion-label>{userStore.state.name}</ion-label>
          </button>
        </Fragment>
      );
    }

    return undefined;
  }

  private renderCloudStatus() {
    if (!navigator.onLine) {
      return undefined;
    }

    const label: string =
      syncStore.state.sync === 'error'
        ? i18n.state.tools.cloud_error
        : syncStore.state.sync === 'in_progress'
        ? i18n.state.tools.cloud_in_progress
        : syncStore.state.sync === 'pending'
          ? i18n.state.tools.cloud_pending
        : i18n.state.tools.cloud_idle;

    return (
      <button class={`cloud ${syncStore.state.sync}`} disabled={true} aria-label={label}>
        {syncStore.state.sync === 'error' ? (
          <ion-icon aria-hidden="true" src="/assets/icons/ionicons/cloud-offline.svg"></ion-icon>
        ) : syncStore.state.sync === 'in_progress' ? (
          <ion-icon aria-hidden="true" src="/assets/icons/ionicons/cloud-upload.svg"></ion-icon>
        ) : syncStore.state.sync === 'pending' ? (
          <ion-icon aria-hidden="true" src="/assets/icons/ionicons/cloud-pending.svg"></ion-icon>
        ) : (
          <ion-icon aria-hidden="true" src="/assets/icons/ionicons/cloud-done.svg"></ion-icon>
        )}
        <ion-label>{i18n.state.tools.cloud}</ion-label>
      </button>
    );
  }
}
