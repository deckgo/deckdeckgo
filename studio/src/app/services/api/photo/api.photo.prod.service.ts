import {ApiPhotoService} from './api.photo.service';

import {EnvironmentUnsplashConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import store from '../../../stores/error.store';

export class ApiPhotoProdService extends ApiPhotoService {
  // @Override
  getPhotos(searchTerm: string, next: string | number): Promise<UnsplashSearchResponse> {
    return new Promise<UnsplashSearchResponse>(async (resolve) => {
      const config: EnvironmentUnsplashConfig = EnvironmentConfigService.getInstance().get('unsplash');

      const searchUrl: string = config.url + 'search/photos/?query=' + searchTerm + '&page=' + next;

      try {
        const rawResponse: Response = await fetch(searchUrl);

        const response: UnsplashSearchResponse = JSON.parse(await rawResponse.text());

        if (!response) {
          store.state.error = 'Unsplash photos could not be fetched';
          resolve();
          return;
        }

        resolve(response);
      } catch (err) {
        store.state.error = err.message;
        resolve();
      }
    });
  }

  // @Override
  registerDownload(photoId: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const config: EnvironmentUnsplashConfig = EnvironmentConfigService.getInstance().get('unsplash');

      const shareUrl: string = config.url + 'photos/' + photoId + '/download/';

      try {
        await fetch(shareUrl);

        // We don't check the status of the answer, user could still use the photo even if that would have failed
        resolve();
      } catch (err) {
        // We ignore the error, user could still use the photo
        resolve();
      }
    });
  }
}
