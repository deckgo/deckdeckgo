import {EnvironmentAppConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';

import {ApiUserProvider} from './api.user.provider';
import {ApiUserMockProvider} from './api.user.mock.provider';
import {ApiUserProdProvider} from './api.user.prod.provider';

export class ApiUserFactoryProvider {
  private static instance: ApiUserProvider;

  static getInstance(): ApiUserProvider {
    if (!ApiUserFactoryProvider.instance) {
      const {mock}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
      ApiUserFactoryProvider.instance = mock ? new ApiUserMockProvider() : new ApiUserProdProvider();
    }
    return ApiUserFactoryProvider.instance;
  }
}
