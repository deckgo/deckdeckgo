import {TenorAnonymousResponse, TenorCategory, TenorCategoryResponse, TenorSearchResponse, throwError} from '@deckdeckgo/editor';
import {get, set} from 'idb-keyval';
import {EnvironmentTenorConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

export class TenorProvider {
  private static instance: TenorProvider;

  static getInstance() {
    if (!TenorProvider.instance) {
      TenorProvider.instance = new TenorProvider();
    }
    return TenorProvider.instance;
  }

  getCategories(): Promise<TenorCategory[]> {
    return new Promise<TenorCategory[]>(async (resolve) => {
      const config: EnvironmentTenorConfig | undefined = EnvironmentConfigService.getInstance().get('tenor');

      if (!config) {
        resolve([]);
        return;
      }

      const anonymousId: string = await this.getAnonymousId();

      const searchUrl: string = config.url + 'categories?key=' + config.key + '&anon_id=' + anonymousId + '&media_filter=minimal';

      try {
        const rawResponse: Response = await fetch(searchUrl);

        const response: TenorCategoryResponse = JSON.parse(await rawResponse.text());

        if (!response) {
          throwError('Tenor trending could not be fetched');
          return;
        }

        resolve(response.tags);
      } catch (err) {
        throwError(err.message);
        resolve([]);
      }
    });
  }

  getGifs(searchTerm: string, next: string | number): Promise<TenorSearchResponse | undefined> {
    return new Promise<TenorSearchResponse | undefined>(async (resolve) => {
      const config: EnvironmentTenorConfig | undefined = EnvironmentConfigService.getInstance().get('tenor');

      if (!config) {
        resolve(undefined);
        return;
      }

      const anonymousId: string = await this.getAnonymousId();

      const searchUrl: string =
        config.url +
        'search?tag=' +
        searchTerm +
        '&key=' +
        config.key +
        '&ar_range=wide&limit=' +
        16 +
        '&anon_id=' +
        anonymousId +
        '&media_filter=minimal&pos=' +
        next;

      try {
        const rawResponse: Response = await fetch(searchUrl);

        const response: TenorSearchResponse = JSON.parse(await rawResponse.text());

        if (!response) {
          throwError('Tenor trending could not be fetched');
          resolve(undefined);
          return;
        }

        resolve(response);
      } catch (err) {
        throwError(err.message);
        resolve(undefined);
      }
    });
  }

  getRandomGif(searchTerm: string): Promise<TenorSearchResponse | undefined> {
    return new Promise<TenorSearchResponse | undefined>(async (resolve) => {
      const config: EnvironmentTenorConfig | undefined = EnvironmentConfigService.getInstance().get('tenor');

      if (!config) {
        resolve(undefined);
        return;
      }

      const anonymousId: string = await this.getAnonymousId();

      const searchUrl: string =
        config.url +
        'random?q=' +
        searchTerm +
        '&key=' +
        config.key +
        '&ar_range=standard&limit=' +
        1 +
        '&anon_id=' +
        anonymousId +
        '&media_filter=minimal';

      try {
        const rawResponse: Response = await fetch(searchUrl);

        const response: TenorSearchResponse | undefined = JSON.parse(await rawResponse.text());

        if (!response) {
          throwError('Tenor trending could not be fetched');
          resolve(undefined);
          return;
        }

        resolve(response);
      } catch (err) {
        // We don't throw an error, in such a case we just not gonna display a gif
        resolve(undefined);
      }
    });
  }

  registerShare(gifId: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const config: EnvironmentTenorConfig | undefined = EnvironmentConfigService.getInstance().get('tenor');

      if (!config) {
        resolve();
        return;
      }

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

      const config: EnvironmentTenorConfig | undefined = EnvironmentConfigService.getInstance().get('tenor');

      if (!config) {
        resolve(localAnonymousId);
        return;
      }

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
