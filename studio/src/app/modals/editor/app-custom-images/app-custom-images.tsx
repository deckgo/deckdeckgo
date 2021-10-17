import {Component, Element, Listen, State, h} from '@stencil/core';

import {StorageFile} from '@deckdeckgo/editor';

import i18n from '../../../stores/i18n.store';

import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';

import {StorageOfflineProvider} from '../../../providers/storage/storage.offline.provider';

import {AppIcon} from '../../../components/core/app-icon/app-icon';

@Component({
  tag: 'app-custom-images',
  styleUrl: 'app-custom-images.scss'
})
export class AppCustomImages {
  @Element() el: HTMLElement;

  private storageOfflineProvider: StorageOfflineProvider;

  private imageHistoryService: ImageHistoryService;

  @State()
  private uploading: boolean = false;

  constructor() {
    this.imageHistoryService = ImageHistoryService.getInstance();
    this.storageOfflineProvider = StorageOfflineProvider.getInstance();
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

  private async selectData(storageFile: StorageFile) {
    if (this.uploading) {
      return;
    }

    await this.selectAndClose(storageFile);
  }

  private async selectAndClose(image: StorageFile) {
    await this.imageHistoryService.push(image);

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(image);
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

        const storageFile: StorageFile = await this.storageOfflineProvider.uploadFile(filePicker.files[0], 'images', 10485760);

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
          <ion-title class="ion-text-uppercase">{i18n.state.editor.your_images}</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        <app-assets
          folder={'images'}
          onSelectAsset={async ($event: CustomEvent<StorageFile>) => await this.selectData($event.detail)}></app-assets>

        <input type="file" accept="image/x-png,image/jpeg,image/gif,image/svg+xml,image/webp" onChange={() => this.upload()} />
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
          <ion-label>{i18n.state.editor.upload_image}</ion-label>
        </ion-button>
      );
    } else {
      return [<ion-spinner color="tertiary"></ion-spinner>, <ion-label class="ion-padding-start">{i18n.state.core.in_progress}</ion-label>];
    }
  }
}
