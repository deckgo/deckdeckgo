import {Component, Element, Listen, State, h} from '@stencil/core';

import {get, set} from 'idb-keyval';

import {IonControllerUtils} from '../../../utils/core/ion-controller-utils';

import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';
import {StorageService} from '../../../services/storage/storage.service';

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

    constructor() {
        this.imageHistoryService = ImageHistoryService.getInstance();
        this.storageService = StorageService.getInstance();
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);

        await this.search();
    }

    @Listen('popstate', { target: 'window' })
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

    private search(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const list: StorageFilesList = await this.storageService.getImages(this.paginationNext);

            if (!list) {
                resolve();
                return;
            }

            if (!list.items || list.items.length <= 0) {
                this.emptyImages();

                resolve();
                return;
            }

            if (!this.imagesOdd) {
                this.imagesOdd = [];
            }

            if (!this.imagesEven) {
                this.imagesEven = [];
            }

            this.imagesOdd = [...this.imagesOdd, ...list.items.filter((_a, i) => !(i % 2))];
            this.imagesEven = [...this.imagesEven, ...list.items.filter((_a, i) => i % 2)];

            this.paginationNext = list.nextPageToken;

            this.disableInfiniteScroll = list.items.length < this.storageService.maxQueryResults;

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

    private async uploadNewImage() {
        const infoDisplayedOnce: boolean = await get<boolean>('deckdeckgo_display_custom_images');

        if (!infoDisplayedOnce) {
            await this.openCustomImagesPublicInfo();
        } else {
            await this.openFilePicker();
        }
    }

    private openFilePicker(): Promise<void> {
        return new Promise<void>((resolve) => {
            const filePicker: HTMLInputElement = this.el.querySelector('input');

            if (!filePicker) {
                resolve();
                return;
            }

            filePicker.click();

            resolve();
        });
    }

    private upload(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const filePicker: HTMLInputElement = this.el.querySelector('input');

            if (!filePicker) {
                resolve();
                return;
            }

            if (filePicker.files && filePicker.files.length > 0) {
                const storageFile: StorageFile = await this.storageService.uploadImage(filePicker.files[0]);

                if (storageFile) {
                    await this.selectAndClose(storageFile);
                }
            }

            resolve();
        });
    }

    private async openCustomImagesPublicInfo() {
        const alert: HTMLIonAlertElement = await IonControllerUtils.createAlert({
            header: 'About your images',
            message: 'Please note that currently, all the images you would upload, will be made public.',
            cssClass: 'custom-images',
            buttons: [
                {
                    text: 'Cancel',
                    role: 'cancel'
                }, {
                    text: 'Ok',
                    handler: async () => {
                        await set('deckdeckgo_display_custom_images', true);

                        await this.openFilePicker();
                    }
                }
            ]
        });

        return await alert.present();
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="tertiary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Add one of your images</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding">
                <app-image-columns imagesOdd={this.imagesOdd} imagesEven={this.imagesEven}
                                  onSelectImage={($event: CustomEvent) => this.selectImage($event)}>
                </app-image-columns>

                <input type="file" accept="image/x-png,image/jpeg,image/gif" onChange={() => this.upload()}/>

                <ion-infinite-scroll threshold="100px" disabled={this.disableInfiniteScroll}
                                     onIonInfinite={(e: CustomEvent<void>) => this.searchNext(e)}>
                    <ion-infinite-scroll-content
                        loadingSpinner="bubbles"
                        loadingText="Loading more data...">
                    </ion-infinite-scroll-content>
                </ion-infinite-scroll>
            </ion-content>,
            <ion-footer>
                <ion-toolbar>
                    <div>
                        <ion-button onClick={() => this.uploadNewImage()} shape="round" color="tertiary">
                            <ion-icon name="cloud-upload" slot="start"></ion-icon>
                            <ion-label>Upload a new image</ion-label>
                        </ion-button>
                    </div>
                </ion-toolbar>
            </ion-footer>
        ];
    }

}
