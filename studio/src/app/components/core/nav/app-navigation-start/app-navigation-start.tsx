import {Component, h, Fragment, Element} from '@stencil/core';

import {loadingController, OverlayEventDetail, popoverController} from '@ionic/core';

import authStore from '../../../../stores/auth.store';
import i18n from '../../../../stores/i18n.store';
import errorStore from '../../../../stores/error.store';
import syncStore from '../../../../stores/sync.store';

import {AppIcon} from '../../app-icon/app-icon';

import {FileSystemService} from '../../../../services/editor/file-system/file-system.service';
import {clearEdit} from '../../../../utils/editor/editor.utils';
import {cloud} from '../../../../utils/core/environment.utils';

@Component({
  tag: 'app-navigation-start',
  styleUrl: 'app-navigation-start.scss',
  shadow: false
})
export class AppNavigationStart {
  @Element() el: HTMLElement;

  private loadInput!: HTMLInputElement;

  private cloud: boolean = cloud();

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
      const type: 'doc' | 'deck' = await FileSystemService.getInstance().importData(this.loadInput.files[0]);

      this.emitReloadEditor(type);
    } catch (err) {
      errorStore.state.error = `Something went wrong. ${err}.`;
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
      errorStore.state.error = 'Something went wrong while cleaning the local data.';
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

  render() {
    return <Fragment>{this.renderActions()}</Fragment>;
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

  private renderShare() {
    if (!this.cloud) {
      return undefined;
    }

    return <app-action-share key="share-action"></app-action-share>;
  }
}
