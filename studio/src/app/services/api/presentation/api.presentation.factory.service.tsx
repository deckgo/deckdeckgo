import {EnvironmentDeckDeckGoConfig} from '../../core/environment/environment-config';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';

import {ApiPresentationService} from './api.presentation.service';
import {ApiPresentationMockService} from './api.presentation.mock.service';
import {ApiPresentationProdService} from './api.presentation.prod.service';

export class ApiPresentationFactoryService {
  private static instance: ApiPresentationService;

  static getInstance() {
    if (!ApiPresentationFactoryService.instance) {
      const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      ApiPresentationFactoryService.instance = !deckDeckGoConfig.prod ? new ApiPresentationMockService() : new ApiPresentationProdService();
    }
    return ApiPresentationFactoryService.instance;
  }
}
