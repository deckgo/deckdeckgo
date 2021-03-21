import {Component, Element, Fragment, h, State} from '@stencil/core';

import i18n from '../../../stores/i18n.store';
import errorStore from '../../../stores/error.store';

import {StorageService} from '../../../services/storage/storage.service';

@Component({
  tag: 'app-deck-import',
  styleUrl: 'app-deck-import.scss',
})
export class AppDeckImport {
  @Element() el: HTMLElement;

  private uploadInput!: HTMLInputElement;

  @State()
  private uploading: boolean = false;

  private storageService: StorageService;

  constructor() {
    this.storageService = StorageService.getInstance();
  }

  async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private openFilePicker() {
    this.uploadInput?.click();
  }

  private async upload() {
    if (!this.uploadInput) {
      this.uploading = false;
      return;
    }

    if (this.uploadInput.files?.length > 0) {
      try {
        await this.uploadFile(this.uploadInput.files[0]);
      } catch (err) {
        errorStore.state.error = 'Data cannot be uploaded.';
      }
    }
  }

  private async uploadFile(file: File) {
    this.uploading = true;

    await this.storageService.uploadFile(file, `decks`, 20971520, false);

    this.uploading = false;
  }

  render() {
    return (
      <Fragment>
        <ion-button onClick={() => this.openFilePicker()} shape="round" color="primary" disabled={this.uploading}>
          <ion-icon name="cloud-upload" slot="start"></ion-icon>
          <ion-label>{i18n.state.editor.upload_data}</ion-label>
        </ion-button>

        <input type="file" accept=".zip" onChange={() => this.upload()} ref={(el) => (this.uploadInput = el as HTMLInputElement)} />
      </Fragment>
    );
  }
}
