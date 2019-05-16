import {Component, Element, Listen, State} from '@stencil/core';

import {PhotoService} from '../../../services/api/photo/photo.service';
import {PhotoHistoryService} from '../../../services/editor/photo-history/photo-history.service';

@Component({
    tag: 'app-photo',
    styleUrl: 'app-photo.scss'
})
export class AppPhoto {

    @Element() el: HTMLElement;

    private photoService: PhotoService;
    private photoHistoryService: PhotoHistoryService;

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
        this.photoService = PhotoService.getInstance();
        this.photoHistoryService = PhotoHistoryService.getInstance();
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('window:popstate')
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

            await this.photoHistoryService.push(photo);

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

    private handleInput($event: CustomEvent<KeyboardEvent>) {
        this.searchTerm = ($event.target as InputTargetEvent).value;
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

            this.photosEven = [...this.photosEven, ...photos.filter((_a, i) => i % 2)];
            this.photosOdd = [...this.photosOdd, ...photos.filter((_a, i) => !(i % 2))];

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

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Pick a photo</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding">
                <app-stock-photos photosOdd={this.photosOdd} photosEven={this.photosEven}
                                  onSelectPhoto={($event: CustomEvent) => this.selectPhoto($event)}>
                </app-stock-photos>

                {this.renderPhotosPlaceHolder()}

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
                    <ion-searchbar debounce={500} placeholder="Search" value={this.searchTerm}
                                   onIonClear={() => this.clear()}
                                   onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
                                   onIonChange={() => {
                                       this.search()
                                   }}></ion-searchbar>
                </ion-toolbar>
            </ion-footer>
        ];
    }

    private renderPhotosPlaceHolder() {
        if ((!this.photosOdd || this.photosOdd.length <= 0) && (!this.photosEven || this.photosEven.length <= 0)) {
            return <div class="photos-placeholder">
                <ion-icon name="images"></ion-icon>
                <ion-label>Photos by Unsplash</ion-label>
            </div>
        } else {
            return undefined;
        }
    }

}
