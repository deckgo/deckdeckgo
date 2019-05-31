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

    push(image: UnsplashPhoto | TenorGif): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!image) {
                resolve();
                return;
            }

            let images: (UnsplashPhoto | TenorGif)[] = await this.get();

            if (!images) {
                images = [];
            }

            const index: number = images.findIndex((filteredPhoto: UnsplashPhoto | TenorGif) => {
               return filteredPhoto.id === image.id
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

    get(): Promise<(UnsplashPhoto | TenorGif)[]> {
        return get('deckdeckgo_images');
    }

}
