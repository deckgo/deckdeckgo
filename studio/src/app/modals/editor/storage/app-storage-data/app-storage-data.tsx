import {StorageFile} from '@deckdeckgo/editor';
import {uploadOfflineFile} from '@deckdeckgo/offline';
import {Component, Element, h, Listen, State} from '@stencil/core';
import {AppIcon} from '../../../../components/core/app-icon/app-icon';
import i18n from '../../../../stores/i18n.store';

@Component({
  tag: 'app-storage-data',
  styleUrl: 'app-storage-data.scss'
})
export class AppStorageData {
  @Element() el: HTMLElement;

  @State()
  private uploading: boolean = false;

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

  private async selectData(storageFile: StorageFile) {
    if (this.uploading) {
      return;
    }

    await this.selectAndClose(storageFile);
  }

  private async selectAndClose(data: StorageFile) {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(data);
  }

  private openFilePicker() {
    const filePicker: HTMLInputElement = this.el.querySelector('input');

    if (!filePicker) {
      return;
    }

    filePicker.click();
  }

  private upload(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const filePicker: HTMLInputElement = this.el.querySelector('input');

      if (!filePicker) {
        this.uploading = false;
        resolve();
        return;
      }

      if (filePicker.files && filePicker.files.length > 0) {
        this.uploading = true;

        const storageFile: StorageFile = await uploadOfflineFile(filePicker.files[0], 'data', 10485760);

        if (storageFile) {
          await this.selectAndClose(storageFile);
        }

        this.uploading = false;
      }

      resolve();
    });
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="tertiary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
              <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">{i18n.state.editor.your_data}</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <app-storage-files
          folder={'data'}
          onSelectAsset={async ($event: CustomEvent<StorageFile>) => await this.selectData($event.detail)}></app-storage-files>

        <input type="file" accept=".csv" onChange={() => this.upload()} />
      </ion-content>,
      <ion-footer>
        <ion-toolbar>
          <div class={this.uploading ? 'uploading' : undefined}>{this.renderToolbarAction()}</div>
        </ion-toolbar>
      </ion-footer>
    ];
  }

  private renderToolbarAction() {
    if (!this.uploading) {
      return (
        <ion-button onClick={() => this.openFilePicker()} shape="round" color="tertiary">
          <AppIcon name="cloud-upload" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
          <ion-label>{i18n.state.editor.upload_data}</ion-label>
        </ion-button>
      );
    } else {
      return [<ion-spinner color="tertiary"></ion-spinner>, <ion-label class="ion-padding-start">{i18n.state.core.in_progress}</ion-label>];
    }
  }
}
