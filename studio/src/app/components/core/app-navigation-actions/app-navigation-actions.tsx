import {Component, h, Fragment, Element} from '@stencil/core';

import {alertController, loadingController, popoverController} from '@ionic/core';

import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';
import i18n from '../../../stores/i18n.store';
import errorStore from '../../../stores/error.store';
import syncStore from '../../../stores/sync.store';
import offlineStore from '../../../stores/offline.store';

import {signIn} from '../../../utils/core/signin.utils';

import {AppIcon} from '../app-icon/app-icon';

import {FileSystemService} from '../../../services/editor/file-system/file-system.service';
import {clearEdit} from '../../../utils/editor/editor.utils';
import {cloud} from '../../../utils/core/environment.utils';

@Component({
  tag: 'app-navigation-actions',
  styleUrl: 'app-navigation-actions.scss',
  shadow: false
})
export class AppNavigationActions {
  @Element() el: HTMLElement;

  private loadInput!: HTMLInputElement;

  private signIn: boolean = cloud();

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
            await this.clear();
          }
        }
      ]
    });

    await alert.present();
  }

  private async clear() {
    const loading: HTMLIonLoadingElement = await loadingController.create({});

    await loading.present();

    try {
      // If the user is logged in, the data might be synced by next cron iteration. Therefore we only clean sync data if user signed out, not when a "New deck" is performed.
      await clearEdit(!authStore.state.loggedIn);

      this.emitReloadDeck();
    } catch (err) {
      errorStore.state.error = 'Something went wrong while cleaning the local data of current deck.';
    }

    await loading.dismiss();
  }

  private emitReloadDeck() {
    const initNewDeck: CustomEvent<void> = new CustomEvent<void>('reloadDeck', {
      bubbles: true
    });

    this.el.dispatchEvent(initNewDeck);
  }

  private async openSyncInfo($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-sync-info',
      mode: 'ios',
      event: $event,
      cssClass: 'info'
    });

    await popover.present();
  }

  render() {
    return (
      <Fragment>
        {this.renderActions()}
        {this.renderSignIn()}
        {this.renderLoggedInActions()}
      </Fragment>
    );
  }

  private renderActions() {
    const disabled: boolean = syncStore.state.sync === 'pending' || syncStore.state.sync === 'in_progress';

    return (
      <Fragment>
        <button
          key="new-deck-action"
          class="ion-activatable"
          onClick={() => this.newDeck()}
          disabled={disabled}
          aria-label={i18n.state.tools.new_presentation}>
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="document" ariaHidden={true} ariaLabel=""></AppIcon>
          <ion-label>{i18n.state.tools.new}</ion-label>
        </button>

        <button key="open-file-action" class="ion-activatable" onClick={() => this.openFilePicker()} disabled={disabled}>
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="folder-open" ariaHidden={true} ariaLabel=""></AppIcon>
          <ion-label>{i18n.state.tools.open}</ion-label>
        </button>

        <input
          type="file"
          accept=".ddg"
          onChange={() => this.importData()}
          ref={(el) => (this.loadInput = el as HTMLInputElement)}
          tabindex="-1"
        />

        <button key="export-action" class="ion-activatable" onClick={() => this.exportData()}>
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="download" ariaHidden={true} ariaLabel=""></AppIcon>
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
      <button key="sign-in-action" class="ion-activatable" onClick={() => signIn()}>
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name="log-in" ariaHidden={true} ariaLabel=""></AppIcon>
        <ion-label>{i18n.state.nav.sign_in}</ion-label>
      </button>
    );
  }

  private renderLoggedInActions() {
    if (authStore.state.loggedIn && userStore.state.loaded) {
      return (
        <Fragment>
          {this.renderCloudStatus()}

          <button
            key="user-menu-action"
            class="ion-activatable"
            onClick={(e: UIEvent) => this.openMenu(e)}
            aria-label={i18n.state.nav.menu}>
            <ion-ripple-effect></ion-ripple-effect>
            <app-avatar src={userStore.state.photoUrl}></app-avatar>
            <ion-label>{userStore.state.name ?? i18n.state.tools.user}</ion-label>
          </button>
        </Fragment>
      );
    }

    return undefined;
  }

  private renderCloudStatus() {
    if (!offlineStore.state.online) {
      return undefined;
    }

    const label: string =
      syncStore.state.sync === 'error'
        ? i18n.state.sync.cloud_error
        : syncStore.state.sync === 'in_progress'
        ? i18n.state.sync.cloud_in_progress
        : syncStore.state.sync === 'pending'
        ? i18n.state.sync.cloud_pending
        : i18n.state.sync.cloud_idle;

    const iconName: string =
      syncStore.state.sync === 'error'
        ? 'cloud-offline'
        : ['in_progress', 'pending'].includes(syncStore.state.sync)
        ? 'sync'
        : 'cloud-done';

    return (
      <button
        key="cloud-status-action"
        class={`cloud ion-activatable ${syncStore.state.sync}`}
        aria-label={label}
        onClick={($event: UIEvent) => this.openSyncInfo($event)}>
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name={iconName} ariaHidden={true} ariaLabel=""></AppIcon>
        <ion-label>{i18n.state.sync.cloud}</ion-label>
      </button>
    );
  }
}
