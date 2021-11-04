import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';

export interface DeckgoImgAttributes {
  src: string;
  label: string;
}

export const extractAttributes = (image?: UnsplashPhoto | TenorGif | StorageFile | Waves): DeckgoImgAttributes | undefined => {
  if (!image || image === undefined) {
    return undefined;
  }

  if (image.hasOwnProperty('urls')) {
    // Unsplash
    const photo: UnsplashPhoto = image as UnsplashPhoto;

    return {
      src: photo.urls.regular,
      label: photo.description ? photo.description : photo.links && photo.links.html ? photo.links.html : photo.urls.regular
    };
  }

  if (image.hasOwnProperty('media')) {
    // Tenor
    const gif: TenorGif = image as TenorGif;

    if (gif.media && gif.media.length > 0 && gif.media[0].gif) {
      return {
        src: gif.media[0].gif.url,
        label: gif.title
      };
    }
  }

  if (image.hasOwnProperty('downloadUrl')) {
    // Storage image aka image uploaded by the user
    const storageFile: StorageFile = image as StorageFile;

    return {
      src: storageFile.downloadUrl,
      label: storageFile.downloadUrl
    };
  }

  return undefined;
};

export const initDeckgoLazyImgAttributes = ({
  element,
  image,
  background = false
}: {
  element: HTMLDeckgoLazyImgElement;
  image: UnsplashPhoto | TenorGif | StorageFile;
  background?: boolean;
}): HTMLDeckgoLazyImgElement => {
  const deckgoImg: DeckgoImgAttributes | undefined = extractAttributes(image);

  if (deckgoImg !== undefined) {
    element.setAttribute('img-src', deckgoImg.src);
    element.setAttribute('img-alt', deckgoImg.label);
  }

  if (image && image !== undefined && image.hasOwnProperty('downloadUrl')) {
    element.customLoader = true;

    // We have to add the information as attributes because slots are going to be cloned to the slides background
    if (background) {
      element.setAttribute('custom-loader', 'true');
    }
  }

  element.setAttribute('contentEditable', 'false');

  return element;
};
