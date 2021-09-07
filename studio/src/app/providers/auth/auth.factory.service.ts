import {EnvironmentAppConfig} from '../../types/core/environment-config';

import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

import {AuthService} from './auth.service';
import {AuthOfflineService} from './auth.offline.service';
import {AuthIcService} from './auth.ic.service';
import {AuthFirebaseService} from './auth.firebase.service';

export class AuthFactoryService {
  private static instance: AuthService;

  static getInstance(): AuthService {
    if (!AuthFactoryService.instance) {
      const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
      AuthFactoryService.instance = cloud === 'offline' ? new AuthOfflineService() : cloud === 'ic' ? new AuthIcService() : new AuthFirebaseService();
    }
    return AuthFactoryService.instance;
  }
}
