import {EnvironmentAppConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';

import {ApiUnsplashProvider} from './api.unsplash.provider';
import {ApiUnsplashMockProvider} from './api.unsplash.mock.provider';
import {ApiUnsplashProdProvider} from './api.unsplash.prod.provider';

export class ApiUnsplashFactoryProvider {
  private static instance: ApiUnsplashProvider;

  static getInstance(): ApiUnsplashProvider {
    if (!ApiUnsplashFactoryProvider.instance) {
      const {mock}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
      ApiUnsplashFactoryProvider.instance = mock ? new ApiUnsplashMockProvider() : new ApiUnsplashProdProvider();
    }
    return ApiUnsplashFactoryProvider.instance;
  }
}
