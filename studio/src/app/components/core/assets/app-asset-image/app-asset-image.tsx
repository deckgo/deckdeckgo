import {h, Component, Host, ComponentInterface, Prop, Listen, State, Fragment} from '@stencil/core';

import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';

@Component({
  tag: 'app-asset-image',
  styleUrl: 'app-asset-image.scss'
})
export class AppAssetImage implements ComponentInterface {
  @Prop()
  image!: UnsplashPhoto | TenorGif | StorageFile | Waves;

  @State()
  private imgLoaded: boolean = false;

  @Listen('innerImgDidLoad')
  onInnerImgDidLoad() {
    this.imgLoaded = true;
  }

  render() {
    return <Host class={`${this.imgLoaded ? 'imgLoaded' : ''}`}>{this.renderImage()}</Host>;
  }

  private renderImage() {
    if (this.image.hasOwnProperty('urls')) {
      return this.renderStockPhoto(this.image as UnsplashPhoto);
    } else if (this.image.hasOwnProperty('media')) {
      return this.renderGif(this.image as TenorGif);
    } else if (this.image.hasOwnProperty('downloadUrl')) {
      return this.renderCustomImage(this.image as StorageFile);
    } else if (this.image.hasOwnProperty('viewBox')) {
      return this.renderWaves(this.image as Waves);
    }

    return undefined;
  }

  private renderGif(gif: TenorGif) {
    if (gif?.media?.[0]?.tinygif?.url) {
      const imgName: string = gif.title ? gif.title : gif.media[0].tinygif.url;

      return (
        <Fragment>
          <div class="image-container">
            <deckgo-lazy-img imgSrc={gif.media[0].tinygif.url} imgAlt={imgName} custom-loader={true}></deckgo-lazy-img>
          </div>
          <ion-label>{imgName}</ion-label>
        </Fragment>
      );
    }

    return undefined;
  }

  private renderPhotoCredits(photo: UnsplashPhoto) {
    if (!photo.user || !photo.user.links || !photo.user.links.html || !photo.user.name) {
      return undefined;
    }

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

  private renderStockPhoto(photo: UnsplashPhoto) {
    if (photo.urls && photo.urls.thumb) {
      const imgName: string = photo.description ? photo.description : photo.links && photo.links.html ? photo.links.html : photo.urls.thumb;

      return (
        <Fragment>
          <div class="image-container">
            <deckgo-lazy-img
              imgSrc={photo.urls.thumb}
              imgAlt={photo.description ? photo.description : photo.links && photo.links.html ? photo.links.html : photo.urls.thumb}
              custom-loader={true}></deckgo-lazy-img>

            {this.renderPhotoCredits(photo)}
          </div>
          <ion-label>{imgName}</ion-label>
        </Fragment>
      );
    }

    return undefined;
  }

  private renderCustomImage(storageFile: StorageFile) {
    if (storageFile && storageFile.downloadUrl) {
      const imgName: string = storageFile.name || storageFile.downloadUrl;

      return (
        <Fragment>
          <div class="image-container">
            <deckgo-lazy-img imgSrc={storageFile.downloadUrl} imgAlt={imgName} custom-loader={true}></deckgo-lazy-img>
          </div>
          <ion-label>{imgName}</ion-label>
        </Fragment>
      );
    }

    return undefined;
  }

  private renderWaves(waves: Waves) {
    if (waves && waves.path) {
      return (
        <div class="image-container">
          <svg {...waves}>
            <path d={waves.path.d} />
          </svg>
        </div>
      );
    }
    return undefined;
  }
}
