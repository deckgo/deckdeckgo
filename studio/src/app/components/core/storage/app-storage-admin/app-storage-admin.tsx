import {StorageFile} from '@deckdeckgo/editor';
import {errorStore} from '@deckdeckgo/studio';
import type {OverlayEventDetail} from '@ionic/core';
import {loadingController, popoverController} from '@ionic/core';
import {Component, ComponentInterface, Event, EventEmitter, h, Host, Prop, State} from '@stencil/core';
import {deleteFile} from '../../../../providers/storage/storage.provider';
import i18n from '../../../../stores/i18n.store';
import {AppIcon} from '../../app-icon/app-icon';

@Component({
  tag: 'app-storage-admin',
  styleUrl: 'app-storage-admin.scss'
})
export class AppStorageAdmin implements ComponentInterface {
  @Prop()
  storageFile!: StorageFile;

  @State()
  private actionInProgress: boolean = false;

  @Event() fileDeleted: EventEmitter<string>;

  private async presentConfirmDelete($event: UIEvent) {
    $event.stopPropagation();

    if (this.actionInProgress) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-delete',
      event: $event,
      mode: 'ios'
    });

    popover.onDidDismiss().then(async ({data}: OverlayEventDetail) => {
      if (data) {
        await this.deleteStorage();
      }
    });

    await popover.present();
  }

  private async deleteStorage() {
    this.actionInProgress = true;

    const loading: HTMLIonLoadingElement = await loadingController.create({});

    await loading.present();

    try {
      await deleteFile(this.storageFile);

      this.fileDeleted.emit(this.storageFile.fullPath);
    } catch (err) {
      errorStore.default.state.error = err;
    }

    await loading.dismiss();

    this.actionInProgress = false;
  }

  render() {
    return (
      <Host>
        <p>{this.storageFile.name}</p>

        <button
          onClick={($event: UIEvent) => this.presentConfirmDelete($event)}
          title={i18n.state.dashboard.delete}
          disabled={this.actionInProgress}
          class={this.actionInProgress ? 'disabled' : undefined}
        >
          <AppIcon name="trash" ariaLabel="" ariaHidden={true}></AppIcon>
        </button>
      </Host>
    );
  }
}
