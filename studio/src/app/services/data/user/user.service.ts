import {User} from '../../../models/data/user';

import {EnvironmentConfigService} from '../../environment/environment-config.service';
import {EnvironmentAppConfig} from '../../../types/core/environment-config';
import {UserIcService} from './user.ic.service';
import {UserFirebaseService} from './user.firebase.service';

export interface UserService {
  update(user: User): Promise<void>;

  delete(userId: string): Promise<void>;
}

export const getUserService = (): UserService => {
  const {cloud} = EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app');

  if (cloud === 'ic') {
    return UserIcService.getInstance();
  } else if (cloud === 'firebase') {
    return UserFirebaseService.getInstance();
  }

  throw new Error('Not implemented');
};
