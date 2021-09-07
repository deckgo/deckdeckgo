import {EnvironmentAppConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';

import {ApiPhotoService} from './api.photo.service';
import {ApiPhotoMockService} from './api.photo.mock.service';
import {ApiPhotoProdService} from './api.photo.prod.service';

export class ApiPhotoFactoryService {
  private static instance: ApiPhotoService;

  static getInstance(): ApiPhotoService {
    if (!ApiPhotoFactoryService.instance) {
      const {mock}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
      ApiPhotoFactoryService.instance = mock ? new ApiPhotoMockService() : new ApiPhotoProdService();
    }
    return ApiPhotoFactoryService.instance;
  }
}
