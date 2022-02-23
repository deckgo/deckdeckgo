import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';
import {Component, ComponentInterface, Fragment, h, Host, Listen, Prop, State} from '@stencil/core';

@Component({
  tag: 'app-asset-image',
  styleUrl: 'app-asset-image.scss'
})
export class AppAssetImage implements ComponentInterface {
  @Prop()
  image!: UnsplashPhoto | TenorGif | StorageFile | Waves | TenorCategory;

  @State()
  private imgLoaded: boolean = false;

  @Listen('innerImgDidLoad')
  onInnerImgDidLoad() {
    this.imgLoaded = true;
  }

  render() {
    const wave: boolean = this.image.hasOwnProperty('viewBox');

    return <Host class={`${this.imgLoaded || wave ? 'imgLoaded' : ''}`}>{this.renderImage()}</Host>;
  }

  private renderImage() {
    if (this.image.hasOwnProperty('urls')) {
      return this.renderStockPhoto(this.image as UnsplashPhoto);
    } else if (this.image.hasOwnProperty('media')) {
      return this.renderGif(this.image as TenorGif);
    } else if (this.image.hasOwnProperty('searchterm')) {
      return this.renderGifCategory(this.image as TenorCategory);
    } else if (this.image.hasOwnProperty('downloadUrl')) {
      return this.renderCustomImage(this.image as StorageFile);
    } else if (this.image.hasOwnProperty('viewBox')) {
      return this.renderWaves(this.image as Waves);
    }

    return undefined;
  }

  private renderGif(gif: TenorGif) {
    if (gif?.media?.[0]?.tinygif?.url) {
      const imgName: string = gif.title || 'GIF';

      return (
        <Fragment>
          <div class="image-container">
            <deckgo-lazy-img imgSrc={gif.media[0].tinygif.url} imgAlt={imgName} custom-loader={true}></deckgo-lazy-img>
          </div>
        </Fragment>
      );
    }

    return undefined;
  }

  private renderGifCategory(category: TenorCategory) {
    return (
      <Fragment>
        <div class="image-container">
          <deckgo-lazy-img imgSrc={category.image} imgAlt={category.name} custom-loader={true}></deckgo-lazy-img>

          <ion-label class="gif-category">{category.name}</ion-label>
        </div>
      </Fragment>
    );
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
          onClick={($event: UIEvent) => $event.stopPropagation()}
        >
          {photo.user.name}
        </a>
      </ion-label>
    );
  }

  private renderStockPhoto(photo: UnsplashPhoto) {
    if (photo.urls && photo.urls.thumb) {
      const imgName: string = photo.description || 'Unsplash';

      return (
        <Fragment>
          <div class="image-container">
            <deckgo-lazy-img imgSrc={photo.urls.thumb} imgAlt={imgName} custom-loader={true}></deckgo-lazy-img>

            {this.renderPhotoCredits(photo)}
          </div>
        </Fragment>
      );
    }

    return undefined;
  }

  private renderCustomImage(storageFile: StorageFile) {
    if (storageFile && storageFile.downloadUrl) {
      return (
        <Fragment>
          <div class="image-container">
            <deckgo-lazy-img imgSrc={storageFile.downloadUrl} imgAlt={storageFile.name} custom-loader={true}></deckgo-lazy-img>
          </div>
        </Fragment>
      );
    }

    return undefined;
  }

  private renderWaves(waves: Waves) {
    if (waves && waves.path) {
      return (
        <div class="image-container waves">
          <svg {...waves}>
            <path d={waves.path.d} />
          </svg>
        </div>
      );
    }
    return undefined;
  }
}
