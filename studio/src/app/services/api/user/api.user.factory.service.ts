import {EnvironmentDeckDeckGoConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../environment/environment-config.service';

import {ApiUserService} from './api.user.service';
import {ApiUserMockService} from './api.user.mock.service';
import {ApiUserProdService} from './api.user.prod.service';

export class ApiUserFactoryService {
  private static instance: ApiUserService;

  static getInstance() {
    if (!ApiUserFactoryService.instance) {
      const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      ApiUserFactoryService.instance = deckDeckGoConfig.mock ? new ApiUserMockService() : new ApiUserProdService();
    }
    return ApiUserFactoryService.instance;
  }
}
