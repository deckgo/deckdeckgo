import {Component, Element, Listen, State, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import {Constants} from '../../../types/core/constants';

import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';
import {StorageService} from '../../../services/storage/storage.service';
import {StorageFactoryService} from '../../../services/storage/storage.factory.service';

import {AppIcon} from '../../../components/core/app-icon/app-icon';

@Component({
  tag: 'app-custom-images',
  styleUrl: 'app-custom-images.scss'
})
export class AppCustomImages {
  @Element() el: HTMLElement;

  private storageService: StorageService;

  private imageHistoryService: ImageHistoryService;

  @State()
  private imagesOdd: StorageFile[];

  @State()
  private imagesEven: StorageFile[];

  @State()
  private disableInfiniteScroll = false;

  private paginationNext: string | null;

  @State()
  private uploading: boolean = false;

  @State()
  private loading: boolean = true;

  @State()
  private folder: string = 'images';

  constructor() {
    this.imageHistoryService = ImageHistoryService.getInstance();
    this.storageService = StorageFactoryService.getInstance();
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

  private selectImage($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      if (this.uploading) {
        resolve();
        return;
      }

      const image: StorageFile = $event.detail;

      await this.selectAndClose(image);

      resolve();
    });
  }

  private selectAndClose(image: StorageFile): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.imageHistoryService.push(image);

      await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(image);

      resolve();
    });
  }

  private search(reset: boolean = false): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const list: StorageFilesList = await this.storageService.getFiles(this.paginationNext, this.folder);

      if (!list) {
        resolve();
        return;
      }

      if (!list.items || list.items.length <= 0) {
        this.emptyImages();

        this.loading = false;

        resolve();
        return;
      }

      if (!this.imagesOdd || reset) {
        this.imagesOdd = [];
      }

      if (!this.imagesEven || reset) {
        this.imagesEven = [];
      }

      this.imagesOdd = [...this.imagesOdd, ...list.items.filter((_a, i) => !(i % 2))];
      this.imagesEven = [...this.imagesEven, ...list.items.filter((_a, i) => i % 2)];

      this.paginationNext = list.nextPageToken;

      this.disableInfiniteScroll = list.items.length < Constants.STORAGE.MAX_QUERY_RESULTS || this.paginationNext === undefined;

      this.loading = false;

      resolve();
    });
  }

  private emptyImages() {
    this.imagesOdd = [];
    this.imagesEven = [];

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

        const storageFile: StorageFile = await this.storageService.uploadFile(filePicker.files[0], 'images', 10485760);

        if (storageFile) {
          await this.selectAndClose(storageFile);
        }

        this.uploading = false;
      }

      resolve();
    });
  }

  private async openFolder($event: CustomEvent<string>) {
    this.loading = true;
    this.folder = $event.detail;

    await this.search(true);
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
        <app-background-folders onSelectFolder={($event: CustomEvent<string>) => this.openFolder($event)}></app-background-folders>

        <app-image-columns
          imagesOdd={this.imagesOdd}
          imagesEven={this.imagesEven}
          onSelectImage={($event: CustomEvent) => this.selectImage($event)}></app-image-columns>

        {this.renderImagesPlaceHolder()}

        <input type="file" accept="image/x-png,image/jpeg,image/gif,image/svg+xml,image/webp" onChange={() => this.upload()} />

        <ion-infinite-scroll threshold="100px" disabled={this.disableInfiniteScroll} onIonInfinite={(e: CustomEvent<void>) => this.searchNext(e)}>
          <ion-infinite-scroll-content loadingText={i18n.state.core.loading}></ion-infinite-scroll-content>
        </ion-infinite-scroll>
      </ion-content>,
      <ion-footer>
        <ion-toolbar>
          <div class={this.uploading ? 'uploading' : undefined}>{this.renderToolbarAction()}</div>
        </ion-toolbar>
      </ion-footer>
    ];
  }

  private renderImagesPlaceHolder() {
    if (this.loading) {
      return undefined;
    }

    if ((!this.imagesOdd || this.imagesOdd.length <= 0) && (!this.imagesEven || this.imagesEven.length <= 0)) {
      return (
        <div class="placeholder">
          <div>
            <AppIcon name="images" ariaLabel="" ariaHidden={true}></AppIcon>
            <ion-label class="ion-text-center">{i18n.state.editor.your_collection_empty}</ion-label>
          </div>
        </div>
      );
    } else {
      return undefined;
    }
  }

  private renderToolbarAction() {
    if (!this.uploading) {
      return (
        <ion-button onClick={() => this.openFilePicker()} shape="round" color="tertiary" disabled={this.folder !== 'images'}>
          <AppIcon name="cloud-upload" ariaLabel="" ariaHidden={true} slot="start"></AppIcon>
          <ion-label>{i18n.state.editor.upload_image}</ion-label>
        </ion-button>
      );
    } else {
      return [<ion-spinner color="tertiary"></ion-spinner>, <ion-label class="ion-padding-start">{i18n.state.core.in_progress}</ion-label>];
    }
  }
}
