import {get, set, del} from 'idb-keyval';

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

  push(image: UnsplashPhoto | TenorGif | StorageFile | SvgWaves): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!image) {
        resolve();
        return;
      }

      let images: (UnsplashPhoto | TenorGif | StorageFile | SvgWaves)[] = await this.get();

      if (!images) {
        images = [];
      }

      const index: number = images.findIndex((filteredPhoto: UnsplashPhoto | TenorGif | StorageFile | SvgWaves) => {
        if (filteredPhoto.hasOwnProperty('fullPath')) {
          return (filteredPhoto as StorageFile).fullPath === (image as StorageFile).fullPath;
        } else if (filteredPhoto.hasOwnProperty('viewBox')) {
          return (filteredPhoto as SvgWaves).path.d === (image as SvgWaves).path.d;
        } else {
          return (filteredPhoto as UnsplashPhoto | TenorGif).id === (image as UnsplashPhoto | TenorGif).id;
        }
      });

      if (index >= 0) {
        resolve();
        return;
      }

      images.unshift(image);

      if (images.length > 10) {
        images.length = 10;
      }

      await set('deckdeckgo_images', images);

      resolve();
    });
  }

  get(): Promise<(UnsplashPhoto | TenorGif | StorageFile | SvgWaves)[]> {
    return get('deckdeckgo_images');
  }
}
