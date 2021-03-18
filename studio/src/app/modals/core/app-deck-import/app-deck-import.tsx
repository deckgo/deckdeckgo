import {Component, Element, Fragment, h, Listen, State} from '@stencil/core';

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

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
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
        <ion-header>
          <ion-toolbar color="primary">
            <ion-buttons slot="start">
              <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
                <ion-icon src="/assets/icons/ionicons/close.svg"></ion-icon>
              </ion-button>
            </ion-buttons>
            <ion-title class="ion-text-uppercase">{i18n.state.dashboard.start_new_presentation}</ion-title>
          </ion-toolbar>
        </ion-header>
        <ion-content class="ion-padding" color="light">
          <ion-button onClick={() => this.openFilePicker()} shape="round" color="primary" disabled={this.uploading}>
            <ion-icon name="cloud-upload" slot="start"></ion-icon>
            <ion-label>{i18n.state.editor.upload_data}</ion-label>
          </ion-button>

          <input type="file" accept=".zip" onChange={() => this.upload()} ref={(el) => (this.uploadInput = el as HTMLInputElement)} />
        </ion-content>
      </Fragment>
    );
  }
}
