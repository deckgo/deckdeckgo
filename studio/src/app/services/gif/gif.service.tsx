import {EnvironmentConfigService} from '../environment/environment-config.service';

import {get, set} from 'idb-keyval';

import {ErrorService} from '../error/error.service';

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

    // call the trending and category endpoints
    getTrending(): Promise<TenorGif[]> {
        return new Promise<TenorGif[]>(async (resolve, reject) => {
            const apiKey: string = EnvironmentConfigService.getInstance().get('tenorKey');
            const searchTerm: string = 'excited';

            const anonymousId: string = await this.getAnonymousId();

            const searchUrl = 'https://api.tenor.com/v1/search?tag=' + searchTerm + '&key=' +
                apiKey + '&ar_range=wide&limit=' + 8 + '&anon_id=' + anonymousId + '&media_filter=minimal';

            try {
                const rawResponse: Response = await fetch(searchUrl);

                const response: TenorTrendingResponse = JSON.parse(await rawResponse.text());

                if (!response) {
                    reject('Tenor trending could not be fetched');
                }

                resolve(response.results);
            } catch (err) {
                this.errorService.error(err.message);
                reject(err);
            }
        });
    }

    private getAnonymousId(): Promise<string> {
        return new Promise<string>(async (resolve, reject) => {
            const localAnonymousId: string = await get('tenor_anonid');

            if (localAnonymousId) {
                resolve(localAnonymousId);
                return;
            }

            const apiKey: string = EnvironmentConfigService.getInstance().get('tenorKey');

            try {
                const rawResponse: Response = await fetch('https://api.tenor.com/v1/anonid?key=' + apiKey);

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
