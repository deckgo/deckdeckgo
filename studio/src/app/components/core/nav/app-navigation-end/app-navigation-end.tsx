import {throwError} from '@deckdeckgo/editor';
import {clearEdit} from '@deckdeckgo/offline';
import {exportData, importData} from '@deckdeckgo/sync';
import {loadingController, OverlayEventDetail, popoverController} from '@ionic/core';
import {Component, Element, Fragment, h, Prop} from '@stencil/core';
import authStore from '../../../../stores/auth.store';
import i18n from '../../../../stores/i18n.store';
import offlineStore from '../../../../stores/offline.store';
import syncStore from '../../../../stores/sync.store';
import userStore from '../../../../stores/user.store';
import {cloud} from '../../../../utils/core/environment.utils';
import {signIn} from '../../../../utils/core/signin.utils';
import {AppIcon} from '../../app-icon/app-icon';

@Component({
  tag: 'app-navigation-end',
  styleUrl: 'app-navigation-end.scss',
  shadow: false
})
export class AppNavigationEnd {
  @Element() el: HTMLElement;

  @Prop()
  editorActions: boolean = false;

  private loadInput!: HTMLInputElement;

  private cloud: boolean = cloud();

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
      await exportData({
        types: [
          {
            description: 'DeckDeckGo Files',
            accept: {
              'application/octet-stream': ['.ddg']
            }
          }
        ]
      });
    } catch (err) {
      throwError(`Something went wrong. ${err}.`);
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
      const type: 'doc' | 'deck' = await importData(this.loadInput.files[0]);

      this.emitReloadEditor(type);
    } catch (err) {
      throwError(`Something went wrong. ${err}.`);
    }

    this.loadInput.value = null;

    await loading.dismiss();
  }

  private async selectType($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-new',
      event: $event,
      mode: 'ios',
      cssClass: 'info'
    });

    popover.onDidDismiss().then(async ({data}: OverlayEventDetail) => {
      if (data === 'deck' || data === 'doc') {
        await this.newType(data);
      }
    });

    await popover.present();
  }

  private async newType(type: 'deck' | 'doc') {
    const loading: HTMLIonLoadingElement = await loadingController.create({});

    await loading.present();

    try {
      // If the user is logged in, the data might be synced by next cron iteration. Therefore we only clean sync data if user signed out, not when a "New deck" is performed.
      await clearEdit(!authStore.state.loggedIn);

      this.emitReloadEditor(type);
    } catch (err) {
      throwError('Something went wrong while cleaning the local data.');
    }

    await loading.dismiss();
  }

  private emitReloadEditor(detail: 'deck' | 'doc') {
    const initNewEditor: CustomEvent<'deck' | 'doc'> = new CustomEvent<'deck' | 'doc'>('reloadEditor', {
      bubbles: true,
      detail
    });

    this.el.dispatchEvent(initNewEditor);
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
        {this.editorActions && this.renderActions()}
        {this.renderSignIn()}
        {this.renderLoggedInActions()}
      </Fragment>
    );
  }

  private renderActions() {
    const disabled: boolean = ['in_progress', 'pending', 'init'].includes(syncStore.state.sync);

    return (
      <Fragment>
        <button
          key="new-select-action"
          class="ion-activatable"
          onClick={($event: UIEvent) => this.selectType($event)}
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

        {this.renderShare()}
      </Fragment>
    );
  }

  private renderSignIn() {
    if (authStore.state.authUser !== null || !this.cloud) {
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

  private renderShare() {
    if (!this.cloud) {
      return undefined;
    }

    return <app-action-share key="share-action"></app-action-share>;
  }

  private renderLoggedInActions() {
    if (authStore.state.authUser !== null) {
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
        : syncStore.state.sync === 'init'
        ? i18n.state.sync.cloud_init
        : syncStore.state.sync === 'pending'
        ? i18n.state.sync.cloud_pending
        : i18n.state.sync.cloud_idle;

    const iconName: string =
      syncStore.state.sync === 'error'
        ? 'cloud-offline'
        : ['in_progress', 'pending', 'init'].includes(syncStore.state.sync)
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
