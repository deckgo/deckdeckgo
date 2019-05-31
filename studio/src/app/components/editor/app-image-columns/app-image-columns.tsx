import {Component, Event, EventEmitter, Prop} from '@stencil/core';

@Component({
    tag: 'app-image-columns',
    styleUrl: 'app-image-columns.scss',
    shadow: true
})
export class AppImageColumns {

    @Prop()
    imagesOdd: (UnsplashPhoto | TenorGif)[];

    @Prop()
    imagesEven: (UnsplashPhoto | TenorGif)[];

    @Event() private selectImage: EventEmitter<UnsplashPhoto | TenorGif>;

    render() {
        if ((!this.imagesEven || this.imagesEven.length <= 0) && (!this.imagesOdd || this.imagesOdd.length <= 0)) {
            return undefined;
        } else {
            return <div class="images-container">
                <div class="images-column">
                    {this.renderImages(this.imagesOdd)}
                </div>
                <div class="images-column">
                    {this.renderImages(this.imagesEven)}
                </div>
            </div>
        }
    }

    private renderImages(images: (UnsplashPhoto | TenorGif)[]) {
        if (images && images.length > 0) {
            return (
                images.map((image: UnsplashPhoto | TenorGif) => {
                    if (image.hasOwnProperty('urls')) {
                        return this.renderStockPhoto(image as UnsplashPhoto);
                    } else if (image.hasOwnProperty('media')) {
                        return this.renderGif(image as TenorGif);
                    } else {
                        return undefined;
                    }
                })
            );
        } else {
            return undefined;
        }
    }

    private renderGif(gif: TenorGif) {
        if (gif && gif.media && gif.media.length > 0 && gif.media[0].gif && gif.media[0].gif.url) {
            return <div class="image ion-padding" custom-tappable onClick={() => this.selectImage.emit(gif)}>
                <div class="image-container">
                    <deckgo-lazy-img imgSrc={gif.media[0].gif.url}
                                     imgAlt={gif.title ? gif.title : gif.media[0].gif.url}></deckgo-lazy-img>
                </div>
            </div>
        } else {
            return undefined;
        }
    }

    private renderStockPhoto(photo: UnsplashPhoto) {
        if (photo.urls && photo.urls.thumb) {
            return <div class="image ion-padding" custom-tappable onClick={() => this.selectImage.emit(photo)}>
                <div class="image-container">
                    <deckgo-lazy-img imgSrc={photo.urls.thumb}
                                     imgAlt={photo.description ? photo.description : (photo.links && photo.links.html ? photo.links.html : photo.urls.thumb)}></deckgo-lazy-img>
                    {this.renderPhotoCredits(photo)}
                </div>
            </div>
        } else {
            return undefined;
        }
    }

    private renderPhotoCredits(photo: UnsplashPhoto) {
        if (!photo.user || !photo.user.links || !photo.user.links.html || !photo.user.name) {
            return undefined;
        } else {
            return <ion-label class="photo-credits"><a
                href={photo.user.links.html + '?utm_source=DeckDeckGo&utm_medium=referral'} target="_blank"
                onClick={($event: UIEvent) => $event.stopPropagation()}>{photo.user.name}</a></ion-label>;
        }
    }

}
