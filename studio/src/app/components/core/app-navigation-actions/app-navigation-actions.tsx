import {Component, Prop, h, Fragment, Element} from '@stencil/core';

import {del} from 'idb-keyval';

import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';
import i18n from '../../../stores/i18n.store';
import errorStore from '../../../stores/error.store';
import syncStore from '../../../stores/sync.store';
import offlineStore from '../../../stores/offline.store';

import {signIn} from '../../../utils/core/signin.utils';
import {alertController, loadingController, popoverController} from '../../../utils/ionic/ionic.overlay';

import {AppIcon} from '../app-icon/app-icon';

import {FileSystemService} from '../../../services/editor/file-system/file-system.service';
import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';
import {SyncService} from '../../../services/editor/sync/sync.service';
import {SyncFactoryService} from '../../../services/editor/sync/sync.factory.service';

@Component({
  tag: 'app-navigation-actions',
  styleUrl: 'app-navigation-actions.scss',
  shadow: false
})
export class AppNavigationActions {
  @Element() el: HTMLElement;

  @Prop() signIn: boolean = false;

  private loadInput!: HTMLInputElement;

  private readonly syncService: SyncService;
  private readonly imageHistoryService: ImageHistoryService;

  constructor() {
    this.syncService = SyncFactoryService.getInstance();
    this.imageHistoryService = ImageHistoryService.getInstance();
  }

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
      await this.syncService.clear();

      await this.imageHistoryService.clear();

      // By removing the reference to the current deck in indexeddb, it will create a new deck on reload
      await del('deckdeckgo_deck_id');

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
    const disabled: boolean = syncStore.state.sync === 'pending' || syncStore.state.sync === 'in_progress';

    return (
      <Fragment>
        <button class="ion-activatable" onClick={() => this.newDeck()} disabled={disabled} aria-label={i18n.state.tools.new_presentation}>
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="document" ariaHidden={true} ariaLabel=""></AppIcon>
          <ion-label>{i18n.state.tools.new}</ion-label>
        </button>

        <button class="ion-activatable" onClick={() => this.openFilePicker()} disabled={disabled}>
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="folder-open" ariaHidden={true} ariaLabel=""></AppIcon>
          <ion-label>{i18n.state.tools.open}</ion-label>
        </button>

        <input type="file" accept=".ddg" onChange={() => this.importData()} ref={(el) => (this.loadInput = el as HTMLInputElement)} tabindex="-1" />

        <button class="ion-activatable" onClick={() => this.exportData()}>
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="download" ariaHidden={true} ariaLabel=""></AppIcon>
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

          <button class="ion-activatable" onClick={(e: UIEvent) => this.openMenu(e)} aria-label={i18n.state.nav.menu}>
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

    return (
      <button class={`cloud ${syncStore.state.sync}`} aria-label={label} onClick={($event: UIEvent) => this.openSyncInfo($event)}>
        {syncStore.state.sync === 'error' ? (
          <AppIcon name="cloud-offline" ariaHidden={true} ariaLabel=""></AppIcon>
        ) : ['in_progress', 'pending'].includes(syncStore.state.sync) ? (
          <AppIcon name="sync" ariaHidden={true} ariaLabel=""></AppIcon>
        ) : (
          <AppIcon name="cloud-done" ariaHidden={true} ariaLabel=""></AppIcon>
        )}
        <ion-label>{i18n.state.sync.cloud}</ion-label>
      </button>
    );
  }
}
