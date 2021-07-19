import {EnvironmentDeckDeckGoConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../environment/environment-config.service';

import {ApiPhotoService} from './api.photo.service';
import {ApiPhotoMockService} from './api.photo.mock.service';
import {ApiPhotoProdService} from './api.photo.prod.service';

export class ApiPhotoFactoryService {
  private static instance: ApiPhotoService;

  static getInstance() {
    if (!ApiPhotoFactoryService.instance) {
      const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      ApiPhotoFactoryService.instance = deckDeckGoConfig.mock ? new ApiPhotoMockService() : new ApiPhotoProdService();
    }
    return ApiPhotoFactoryService.instance;
  }
}
