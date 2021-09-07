import {EnvironmentAppConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';

import {ApiUserService} from './api.user.service';
import {ApiUserMockService} from './api.user.mock.service';
import {ApiUserProdService} from './api.user.prod.service';

export class ApiUserFactoryService {
  private static instance: ApiUserService;

  static getInstance(): ApiUserService {
    if (!ApiUserFactoryService.instance) {
      const {mock}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
      ApiUserFactoryService.instance = mock ? new ApiUserMockService() : new ApiUserProdService();
    }
    return ApiUserFactoryService.instance;
  }
}
