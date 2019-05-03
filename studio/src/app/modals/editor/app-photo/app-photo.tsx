import {Component, Element, Listen, State} from '@stencil/core';

import {PhotoService} from '../../../services/api/photo/photo.service';

@Component({
    tag: 'app-photo',
    styleUrl: 'app-photo.scss'
})
export class AppPhoto {

    @Element() el: HTMLElement;

    private photoService: PhotoService;

    @State()
    private photosOdd: PixabayHit[];

    @State()
    private photosEven: PixabayHit[];

    @State()
    private searchTerm: string;

    private previousSearchTerm: string;

    @State()
    private disableInfiniteScroll = false;

    private paginationNext: number = 1;

    constructor() {
        this.photoService = PhotoService.getInstance();
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

    private selectPhoto(photo: PixabayHit): Promise<void> {
        return new Promise<void>(async (resolve) => {
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

            const pixabayResponse: PixabaySearchResponse = await this.photoService.getPhotos(this.searchTerm, this.paginationNext);

            if (!pixabayResponse) {
                resolve();
                return;
            }

            const photos: PixabayHit[] = pixabayResponse.hits;

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

            this.photosOdd = [...this.photosOdd, ...photos.filter((_a, i) => i % 2)];
            this.photosEven = [...this.photosEven, ...photos.filter((_a, i) => !(i % 2))];

            if (!this.paginationNext || this.paginationNext === 0 || newSearchTerm) {
                // We just put a small delay because of the repaint
                setTimeout(async () => {
                    await this.autoScrollToTop();
                }, 100)
            }

            this.disableInfiniteScroll = this.paginationNext * 20 >= pixabayResponse.totalHits;

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
                    <ion-title class="ion-text-uppercase">Pick a stock photo</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding">
                <div class="photos-container">
                    <div class="photos-column">
                        {this.renderPhotos(this.photosOdd)}
                    </div>
                    <div class="photos-column">
                        {this.renderPhotos(this.photosEven)}
                    </div>
                </div>

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
                    <ion-searchbar debounce={500} placeholder="Search Pixabay" value={this.searchTerm}
                                   onIonClear={() => this.clear()}
                                   onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
                                   onIonChange={() => {this.search()}}></ion-searchbar>
                </ion-toolbar>
            </ion-footer>
        ];
    }

    private renderPhotos(photos: PixabayHit[]) {
        if (photos && photos.length > 0) {
            return (
                photos.map((photo: PixabayHit) => {
                    if (photo.previewURL) {
                        return <div class="photo ion-padding" custom-tappable onClick={() => this.selectPhoto(photo)}>
                            <div class="photo-container">
                                <img src={photo.previewURL} alt={photo.tags ? photo.tags : photo.previewURL}></img>
                            </div>
                        </div>
                    } else {
                        return undefined;
                    }
                })
            );
        } else {
            return undefined;
        }
    }

}
