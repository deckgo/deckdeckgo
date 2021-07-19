import {EnvironmentDeckDeckGoConfig} from '../../types/core/environment-config';

import {EnvironmentConfigService} from '../environment/environment-config.service';

import {AuthService} from './auth.service';
import {AuthNoneService} from './auth.none.service';
import {AuthIcService} from './auth.ic.service';
import {AuthFirebaseService} from './auth.firebase.service';

export class AuthFactoryService {
  private static instance: AuthService;

  static getInstance(): AuthService {
    if (!AuthFactoryService.instance) {
      const {cloud}: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      AuthFactoryService.instance = cloud === 'none' ? new AuthNoneService() : cloud === 'ic' ? new AuthIcService() : new AuthFirebaseService();
    }
    return AuthFactoryService.instance;
  }
}
