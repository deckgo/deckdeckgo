import {get, set, del} from 'idb-keyval';

import {Reference} from '@firebase/storage-types';

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

    push(image: UnsplashPhoto | TenorGif | Reference): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!image) {
                resolve();
                return;
            }

            let images: (UnsplashPhoto | TenorGif | Reference)[] = await this.get();

            if (!images) {
                images = [];
            }

            const index: number = images.findIndex((filteredPhoto: UnsplashPhoto | TenorGif | Reference) => {
                if ('fullPath' in filteredPhoto) {
                    return (filteredPhoto as Reference).fullPath === (image as Reference).fullPath;
                } else {
                    return (filteredPhoto as UnsplashPhoto | TenorGif).id === (image as UnsplashPhoto | TenorGif).id
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

    get(): Promise<(UnsplashPhoto | TenorGif | Reference)[]> {
        return get('deckdeckgo_images');
    }

}
