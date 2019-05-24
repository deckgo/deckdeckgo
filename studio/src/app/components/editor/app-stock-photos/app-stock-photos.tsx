import {Component, Event, EventEmitter, Prop} from '@stencil/core';

@Component({
    tag: 'app-stock-photos',
    styleUrl: 'app-stock-photos.scss',
    shadow: true
})
export class AppStockPhotos {

    @Prop()
    photosOdd: UnsplashPhoto[];

    @Prop()
    photosEven: UnsplashPhoto[];

    @Event() private selectPhoto: EventEmitter<UnsplashPhoto>;

    render() {
        return <div class="photos-container">
            <div class="photos-column">
                {this.renderPhotos(this.photosOdd)}
            </div>
            <div class="photos-column">
                {this.renderPhotos(this.photosEven)}
            </div>
        </div>
    }

    private renderPhotos(photos: UnsplashPhoto[]) {
        if (photos && photos.length > 0) {
            return (
                photos.map((photo: UnsplashPhoto) => {
                    if (photo.urls && photo.urls.thumb) {
                        return <div class="photo ion-padding" custom-tappable onClick={() => this.selectPhoto.emit(photo)}>
                            <div class="photo-container">
                                <deckgo-lazy-img imgSrc={photo.urls.thumb}
                                     imgAlt={photo.description ? photo.description : (photo.links && photo.links.html ? photo.links.html : photo.urls.thumb)}></deckgo-lazy-img>
                                {this.renderPhotoCredits(photo)}
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
