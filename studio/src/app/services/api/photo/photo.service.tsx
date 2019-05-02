import {EnvironmentPixabayConfig} from '../../core/environment/environment-config';

import {ErrorService} from '../../core/error/error.service';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

export class PhotoService {

    private static instance: PhotoService;

    private errorService: ErrorService;

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
    }

    static getInstance() {
        if (!PhotoService.instance) {
            PhotoService.instance = new PhotoService();
        }
        return PhotoService.instance;
    }

    getPhotos(searchTerm: string, next: string | number): Promise<PixabaySearchResponse> {
        return new Promise<PixabaySearchResponse>(async (resolve) => {
            const config: EnvironmentPixabayConfig = EnvironmentConfigService.getInstance().get('pixabay');

            const searchUrl: string = config.url + '?q=' + searchTerm + '&key=' + config.key + '&page=' + next;

            try {
                const rawResponse: Response = await fetch(searchUrl);

                const response: PixabaySearchResponse = JSON.parse(await rawResponse.text());

                if (!response) {
                    this.errorService.error('Pixabay photos could not be fetched');
                    resolve();
                    return;
                }

                resolve(response);
            } catch (err) {
                this.errorService.error(err.message);
                resolve();
            }
        });
    }

}
