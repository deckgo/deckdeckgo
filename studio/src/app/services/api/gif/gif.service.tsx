import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import {get, set} from 'idb-keyval';

import {ErrorService} from '../../core/error/error.service';
import {EnvironmentTenorConfig} from '../../core/environment/environment-config';

export class GifService {

    private static instance: GifService;

    private errorService: ErrorService;

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
    }

    static getInstance() {
        if (!GifService.instance) {
            GifService.instance = new GifService();
        }
        return GifService.instance;
    }

    getCategories(): Promise<TenorCategory[]> {
        return new Promise<TenorCategory[]>(async (resolve) => {
            const config: EnvironmentTenorConfig = EnvironmentConfigService.getInstance().get('tenor');

            const anonymousId: string = await this.getAnonymousId();

            const searchUrl: string = config.url + 'categories?key=' + config.key + '&anon_id=' + anonymousId + '&media_filter=minimal';

            try {
                const rawResponse: Response = await fetch(searchUrl);

                const response: TenorCategoryResponse = JSON.parse(await rawResponse.text());

                if (!response) {
                    this.errorService.error('Tenor trending could not be fetched');
                    return;
                }

                resolve(response.tags);
            } catch (err) {
                this.errorService.error(err.message);
                resolve(err);
            }
        });
    }

    getGifs(searchTerm: string, next: string | number): Promise<TenorSearchResponse> {
        return new Promise<TenorSearchResponse>(async (resolve) => {
            const config: EnvironmentTenorConfig = EnvironmentConfigService.getInstance().get('tenor');

            const anonymousId: string = await this.getAnonymousId();

            const searchUrl: string = config.url + 'search?tag=' + searchTerm + '&key=' +
                config.key + '&ar_range=wide&limit=' + 16 + '&anon_id=' + anonymousId + '&media_filter=minimal&pos=' + next;

            try {
                const rawResponse: Response = await fetch(searchUrl);

                const response: TenorSearchResponse = JSON.parse(await rawResponse.text());

                if (!response) {
                    this.errorService.error('Tenor trending could not be fetched');
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

    registerShare(gifId: string): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const config: EnvironmentTenorConfig = EnvironmentConfigService.getInstance().get('tenor');

            // It isn't mandatory to provide the anonymous ID therefore, as we rather not like to track even if anonymous, we don't provide it

            const shareUrl: string = config.url + 'registershare?key=' + config.key + '&id=' + gifId;

            try {
                await fetch(shareUrl);

                // We don't check the status of the answer, user could still use the Gifs even if that would have failed
                resolve();
            } catch (err) {
                // We ignore the error, user could still use the Gifs
                resolve();
            }
        });
    }

    private getAnonymousId(): Promise<string> {
        return new Promise<string>(async (resolve, reject) => {
            const localAnonymousId: string = await get<string>('tenor_anonid');

            if (localAnonymousId) {
                resolve(localAnonymousId);
                return;
            }

            const config: EnvironmentTenorConfig = EnvironmentConfigService.getInstance().get('tenor');

            try {
                const rawResponse: Response = await fetch(config.url + 'anonid?key=' + config.key);

                const response: TenorAnonymousResponse = JSON.parse(await rawResponse.text());

                if (!response) {
                    reject('Tenor anonymous ID could not be fetched');
                }

                const anonymousId: string = response.anon_id;

                await set('tenor_anonid', anonymousId);

                resolve(anonymousId);
            } catch (err) {
                reject(err);
            }
        });
    }
}
