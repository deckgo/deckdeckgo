import {Component, Element, Listen, State, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import {Constants} from '../../../types/core/constants';

import {StorageService} from '../../../services/storage/storage.service';

@Component({
  tag: 'app-custom-data',
  styleUrl: 'app-custom-data.scss',
})
export class AppCustomData {
  @Element() el: HTMLElement;

  private storageService: StorageService;

  @State()
  private files: StorageFile[];

  @State()
  private disableInfiniteScroll = false;

  private paginationNext: string | null;

  @State()
  private uploading: boolean = false;

  @State()
  private loading: boolean = true;

  constructor() {
    this.storageService = StorageService.getInstance();
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);

    await this.search();
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  private selectData(storageFile: StorageFile): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!storageFile) {
        resolve();
        return;
      }

      if (this.uploading) {
        resolve();
        return;
      }

      await this.selectAndClose(storageFile);

      resolve();
    });
  }

  private selectAndClose(data: StorageFile): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(data);

      resolve();
    });
  }

  private search(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const list: StorageFilesList = await this.storageService.getFiles(this.paginationNext, 'data');

      if (!list) {
        resolve();
        return;
      }

      if (!list.items || list.items.length <= 0) {
        this.emptyFiles();

        this.loading = false;

        resolve();
        return;
      }

      if (!this.files) {
        this.files = [];
      }

      this.files = [...this.files, ...list.items];

      this.paginationNext = list.nextPageToken;

      this.disableInfiniteScroll = list.items.length < Constants.STORAGE.MAX_QUERY_RESULTS || this.paginationNext === undefined;

      this.loading = false;

      resolve();
    });
  }

  private emptyFiles() {
    this.files = [];

    this.disableInfiniteScroll = true;
  }

  private searchNext(e: CustomEvent<void>): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.search();

      (e.target as HTMLIonInfiniteScrollElement).complete();

      resolve();
    });
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

        const storageFile: StorageFile = await this.storageService.uploadFile(filePicker.files[0], 'data', 10485760);

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
              <ion-icon src="/assets/icons/ionicons/close.svg"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">Your data</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        {this.renderData()}

        <input type="file" accept=".csv" onChange={() => this.upload()} />

        <ion-infinite-scroll threshold="100px" disabled={this.disableInfiniteScroll} onIonInfinite={(e: CustomEvent<void>) => this.searchNext(e)}>
          <ion-infinite-scroll-content loadingText="Loading more images..."></ion-infinite-scroll-content>
        </ion-infinite-scroll>
      </ion-content>,
      <ion-footer>
        <ion-toolbar>
          <div class={this.uploading ? 'uploading' : undefined}>{this.renderToolbarAction()}</div>
        </ion-toolbar>
      </ion-footer>,
    ];
  }

  private renderData() {
    if (!this.files || this.files.length <= 0) {
      return this.renderPlaceHolder();
    } else {
      return <div class="data-container">{this.renderFiles()}</div>;
    }
  }

  private renderFiles() {
    return this.files.map((storageFile: StorageFile) => {
      return this.renderFile(storageFile);
    });
  }

  private renderFile(storageFile: StorageFile) {
    return (
      <div class="ion-padding data" custom-tappable onClick={() => this.selectData(storageFile)}>
        <ion-icon src="/assets/icons/file.svg"></ion-icon>
        <ion-label>{storageFile.name}</ion-label>
      </div>
    );
  }

  private renderPlaceHolder() {
    if (this.loading) {
      return undefined;
    }

    return (
      <div class="placeholder">
        <div>
          <ion-icon src="/assets/icons/file.svg"></ion-icon>
          <ion-label class="ion-text-center">Your collection of data is empty</ion-label>
        </div>
      </div>
    );
  }

  private renderToolbarAction() {
    if (!this.uploading) {
      return (
        <ion-button onClick={() => this.openFilePicker()} shape="round" color="tertiary">
          <ion-icon name="cloud-upload" slot="start"></ion-icon>
          <ion-label>Upload a new data</ion-label>
        </ion-button>
      );
    } else {
      return [<ion-spinner color="tertiary"></ion-spinner>, <ion-label class="ion-padding-start">Upload in progress</ion-label>];
    }
  }
}
