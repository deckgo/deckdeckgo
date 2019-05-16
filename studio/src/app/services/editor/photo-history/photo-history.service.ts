import {get, set, del} from 'idb-keyval';

export class PhotoHistoryService {

    private static instance: PhotoHistoryService;

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!PhotoHistoryService.instance) {
            PhotoHistoryService.instance = new PhotoHistoryService();
        }
        return PhotoHistoryService.instance;
    }

    clear(): Promise<void> {
        return del('deckdeckgo_photos');
    }

    push(photo: UnsplashPhoto): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!photo) {
                resolve();
                return;
            }

            let photos: UnsplashPhoto[] = await this.get();

            if (!photos) {
                photos = [];
            }

            const index: number = photos.findIndex((filteredPhoto: UnsplashPhoto) => {
               return filteredPhoto.id === photo.id
            });

            if (index >= 0) {
                resolve();
                return;
            }

            photos.unshift(photo);

            if (photos.length > 10) {
                photos.length = 10;
            }

            await set('deckdeckgo_photos', photos);

            resolve();
        });
    }

    get(): Promise<UnsplashPhoto[]> {
        return get('deckdeckgo_photos');
    }

}
