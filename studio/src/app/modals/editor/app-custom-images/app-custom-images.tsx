import {Component, Element, Listen, State, h} from '@stencil/core';

import {ApiPhotoService} from '../../../services/api/photo/api.photo.service';
import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';
import {StorageService} from '../../../services/storage/storage.service';

@Component({
    tag: 'app-custom-images',
    styleUrl: 'app-custom-images.scss'
})
export class AppCustomImages {

    @Element() el: HTMLElement;

    private storageService: StorageService;

    private photoService: ApiPhotoService;
    private imageHistoryService: ImageHistoryService;

    @State()
    private photosOdd: UnsplashPhoto[];

    @State()
    private photosEven: UnsplashPhoto[];

    @State()
    private searchTerm: string;

    private previousSearchTerm: string;

    @State()
    private disableInfiniteScroll = false;

    private paginationNext: number = 1;

    constructor() {
        this.photoService = ApiPhotoService.getInstance();
        this.imageHistoryService = ImageHistoryService.getInstance();
        this.storageService = StorageService.getInstance();
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('popstate', { target: 'window' })
    async handleHardwareBackButton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private selectPhoto($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event || !$event.detail) {
                resolve();
                return;
            }

            const photo: UnsplashPhoto = $event.detail;

            await this.photoService.registerDownload(photo.id);

            await this.imageHistoryService.push(photo);

            await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(photo);

            resolve();
        });
    }

    private clear(): Promise<void> {
        return new Promise<void>((resolve) => {
            this.photosOdd = null;
            this.photosEven = null;

            this.disableInfiniteScroll = false;

            this.paginationNext = 1;

            resolve();
        });
    }

    private search(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.searchTerm || this.searchTerm.length <= 0) {
                await this.clear();
                resolve();
                return;
            }

            const unsplashResponse: UnsplashSearchResponse = await this.photoService.getPhotos(this.searchTerm, this.paginationNext);

            if (!unsplashResponse) {
                resolve();
                return;
            }

            const photos: UnsplashPhoto[] = unsplashResponse.results;

            if (!photos || photos.length <= 0) {
                this.emptyPhotos();

                resolve();
                return;
            }

            if (!this.photosOdd) {
                this.photosOdd = [];
            }

            if (!this.photosEven) {
                this.photosEven = [];
            }

            const newSearchTerm: boolean = !this.previousSearchTerm || this.searchTerm !== this.previousSearchTerm;

            if (newSearchTerm) {
                this.photosOdd = [];
                this.photosEven = [];
            }

            this.photosOdd = [...this.photosOdd, ...photos.filter((_a, i) => !(i % 2))];
            this.photosEven = [...this.photosEven, ...photos.filter((_a, i) => i % 2)];

            if (!this.paginationNext || this.paginationNext === 0 || newSearchTerm) {
                // We just put a small delay because of the repaint
                setTimeout(async () => {
                    await this.autoScrollToTop();
                }, 100)
            }

            this.disableInfiniteScroll = this.paginationNext * 10 >= unsplashResponse.total;

            this.paginationNext++;

            this.previousSearchTerm = this.searchTerm;

            resolve();
        });
    }

    private autoScrollToTop(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const content: HTMLIonContentElement = this.el.querySelector('ion-content');

            if (!content) {
                resolve();
                return;
            }

            await content.scrollToTop();

            resolve();
        });
    }

    private emptyPhotos() {
        this.photosOdd = [];
        this.photosEven = [];

        this.disableInfiniteScroll = true;
    }

    private searchNext(e: CustomEvent<void>): Promise<void> {
        return new Promise<void>(async (resolve) => {
            await this.search();

            (e.target as HTMLIonInfiniteScrollElement).complete();

            resolve();
        });
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
                await this.storageService.uploadImage(filePicker.files[0]);
            }

            resolve();
        });
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Pick one of your images</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding">
                <app-image-columns imagesOdd={this.photosOdd} imagesEven={this.photosEven}
                                  onSelectImage={($event: CustomEvent) => this.selectPhoto($event)}>
                </app-image-columns>

                <ion-button onClick={() => this.openFilePicker()}>Upload</ion-button>

                <input type="file" accept="image/x-png,image/jpeg,image/gif" onChange={() => this.upload()}/>

                <ion-infinite-scroll threshold="100px" disabled={this.disableInfiniteScroll}
                                     onIonInfinite={(e: CustomEvent<void>) => this.searchNext(e)}>
                    <ion-infinite-scroll-content
                        loadingSpinner="bubbles"
                        loadingText="Loading more data...">
                    </ion-infinite-scroll-content>
                </ion-infinite-scroll>
            </ion-content>
        ];
    }

}
