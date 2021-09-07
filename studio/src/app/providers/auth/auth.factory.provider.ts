import {EnvironmentAppConfig} from '../../types/core/environment-config';

import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

import {AuthProvider} from './auth.service';
import {AuthOfflineProvider} from './auth.offline.service';
import {AuthIcProvider} from './auth.ic.service';
import {AuthFirebaseProvider} from './auth.firebase.service';

export class AuthFactoryProvider {
  private static instance: AuthProvider;

  static getInstance(): AuthProvider {
    if (!AuthFactoryProvider.instance) {
      const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
      AuthFactoryProvider.instance = cloud === 'offline' ? new AuthOfflineProvider() : cloud === 'ic' ? new AuthIcProvider() : new AuthFirebaseProvider();
    }
    return AuthFactoryProvider.instance;
  }
}
