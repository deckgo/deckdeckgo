import {Component, Event, EventEmitter, Prop, h} from '@stencil/core';

import {StorageFile} from '@deckdeckgo/editor';

@Component({
  tag: 'app-image-columns',
  styleUrl: 'app-image-columns.scss',
  shadow: true
})
export class AppImageColumns {
  @Prop()
  imagesOdd: (UnsplashPhoto | TenorGif | StorageFile | Waves)[];

  @Prop()
  imagesEven: (UnsplashPhoto | TenorGif | StorageFile | Waves)[];

  @Event() private selectImage: EventEmitter<UnsplashPhoto | TenorGif | StorageFile | Waves>;

  render() {
    if ((!this.imagesEven || this.imagesEven.length <= 0) && (!this.imagesOdd || this.imagesOdd.length <= 0)) {
      return undefined;
    } else {
      return (
        <div class="images-container">
          <div class="images-column">{this.renderImages(this.imagesOdd)}</div>
          <div class="images-column">{this.renderImages(this.imagesEven)}</div>
        </div>
      );
    }
  }

  private renderImages(images: (UnsplashPhoto | TenorGif | StorageFile | Waves)[]) {
    if (images && images.length > 0) {
      return images.map((image: UnsplashPhoto | TenorGif | StorageFile | Waves) => {
        if (image.hasOwnProperty('urls')) {
          return this.renderStockPhoto(image as UnsplashPhoto);
        } else if (image.hasOwnProperty('media')) {
          return this.renderGif(image as TenorGif);
        } else if (image.hasOwnProperty('downloadUrl')) {
          return this.renderCustomImage(image as StorageFile);
        } else if (image.hasOwnProperty('viewBox')) {
          return this.renderWaves(image as Waves);
        } else {
          return undefined;
        }
      });
    } else {
      return undefined;
    }
  }

  private renderGif(gif: TenorGif) {
    if (gif?.media?.[0]?.tinygif?.url) {
      return (
        <div class="image ion-padding" custom-tappable onClick={() => this.selectImage.emit(gif)}>
          <div class="image-container">
            <deckgo-lazy-img
              imgSrc={gif.media[0].tinygif.url}
              imgAlt={gif.title ? gif.title : gif.media[0].tinygif.url}
              custom-loader={true}></deckgo-lazy-img>
          </div>
        </div>
      );
    } else {
      return undefined;
    }
  }

  private renderStockPhoto(photo: UnsplashPhoto) {
    if (photo.urls && photo.urls.thumb) {
      return (
        <div class="image ion-padding" custom-tappable onClick={() => this.selectImage.emit(photo)}>
          <div class="image-container">
            <deckgo-lazy-img
              imgSrc={photo.urls.thumb}
              imgAlt={photo.description ? photo.description : photo.links && photo.links.html ? photo.links.html : photo.urls.thumb}
              custom-loader={true}></deckgo-lazy-img>
            {this.renderPhotoCredits(photo)}
          </div>
        </div>
      );
    } else {
      return undefined;
    }
  }

  private renderPhotoCredits(photo: UnsplashPhoto) {
    if (!photo.user || !photo.user.links || !photo.user.links.html || !photo.user.name) {
      return undefined;
    } else {
      return (
        <ion-label class="photo-credits">
          <a
            href={photo.user.links.html + '?utm_source=DeckDeckGo&utm_medium=referral'}
            target="_blank"
            rel="noopener noreferrer"
            onClick={($event: UIEvent) => $event.stopPropagation()}>
            {photo.user.name}
          </a>
        </ion-label>
      );
    }
  }

  private renderCustomImage(storageFile: StorageFile) {
    if (storageFile && storageFile.downloadUrl) {
      return (
        <div class="image ion-padding" custom-tappable onClick={() => this.selectImage.emit(storageFile)}>
          <div class="image-container">
            <deckgo-lazy-img imgSrc={storageFile.downloadUrl} imgAlt={storageFile.downloadUrl} custom-loader={true}></deckgo-lazy-img>
          </div>
        </div>
      );
    } else {
      return undefined;
    }
  }

  private renderWaves(waves: Waves) {
    if (waves && waves.path) {
      return (
        <div class="image ion-padding" custom-tappable onClick={() => this.selectImage.emit(waves)}>
          <div class="image-container">
            <svg {...waves}>
              <path d={waves.path.d} />
            </svg>
          </div>
        </div>
      );
    } else {
      return undefined;
    }
  }
}
