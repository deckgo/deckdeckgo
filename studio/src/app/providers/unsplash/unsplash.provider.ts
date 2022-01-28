import {UnsplashPhoto, RegisterUnsplashDownload, UnsplashSearchResponse, GetUnsplashPhotos} from '@deckdeckgo/editor';

import {EnvironmentUnsplashConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

import {unsplashProvider} from '../../utils/core/providers.utils';
import {unsplash} from '../../utils/core/environment.utils';

export const getUnsplashPhotos = async ({
  searchTerm,
  paginationNext
}: {
  searchTerm: string;
  paginationNext: number;
}): Promise<UnsplashSearchResponse | undefined> => {
  if (!unsplash()) {
    return Promise.resolve(undefined);
  }

  const {url}: EnvironmentUnsplashConfig = EnvironmentConfigService.getInstance().get('unsplash');

  const {getUnsplashPhotos: getPhotos}: {getUnsplashPhotos: GetUnsplashPhotos} = await unsplashProvider<{
    getUnsplashPhotos: GetUnsplashPhotos;
  }>();

  return getPhotos({
    apiUrl: url,
    searchTerm,
    next: paginationNext
  });
};

export const registerUnsplashDownload = async (photo: UnsplashPhoto) => {
  if (!unsplash()) {
    return;
  }

  const {url}: EnvironmentUnsplashConfig = EnvironmentConfigService.getInstance().get('unsplash');

  const {registerUnsplashDownload: registerDownload}: {registerUnsplashDownload: RegisterUnsplashDownload} = await unsplashProvider<{
    registerUnsplashDownload: RegisterUnsplashDownload;
  }>();

  await registerDownload({
    apiUrl: url,
    photoId: photo.id
  });
};
