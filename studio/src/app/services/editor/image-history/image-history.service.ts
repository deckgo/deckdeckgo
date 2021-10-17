import {get, set, del} from 'idb-keyval';

import {StorageFile, UnsplashPhoto} from '@deckdeckgo/editor';

export class ImageHistoryService {
  private static instance: ImageHistoryService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!ImageHistoryService.instance) {
      ImageHistoryService.instance = new ImageHistoryService();
    }
    return ImageHistoryService.instance;
  }

  clear(): Promise<void> {
    return del('deckdeckgo_images');
  }

  async push(image: UnsplashPhoto | TenorGif | StorageFile | Waves) {
    if (!image) {
      return;
    }

    let images: (UnsplashPhoto | TenorGif | StorageFile | Waves)[] = await this.get();

    if (!images) {
      images = [];
    }

    // We always add waves in the history
    const exist: boolean = image.hasOwnProperty('viewBox') ? false : this.exists(images, image);

    if (exist) {
      return;
    }

    images.unshift(image);

    if (images.length > 50) {
      images.length = 50;
    }

    await set('deckdeckgo_images', images);
  }

  get(): Promise<(UnsplashPhoto | TenorGif | StorageFile)[]> {
    return get('deckdeckgo_images');
  }

  private exists(
    images: (UnsplashPhoto | TenorGif | StorageFile | Waves)[],
    image: UnsplashPhoto | TenorGif | StorageFile | Waves
  ): boolean {
    const index: number = images.findIndex((filteredPhoto: UnsplashPhoto | TenorGif | StorageFile) => {
      if (filteredPhoto.hasOwnProperty('fullPath')) {
        return (filteredPhoto as StorageFile).fullPath === (image as StorageFile).fullPath;
      } else {
        return (filteredPhoto as UnsplashPhoto | TenorGif).id === (image as UnsplashPhoto | TenorGif).id;
      }
    });

    return index >= 0;
  }
}
