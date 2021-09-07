import {User} from '../../../models/data/user';

import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';
import {EnvironmentAppConfig} from '../../../types/core/environment-config';
import {UserIcProvider} from './user.ic.provider';
import {UserFirebaseProvider} from './user.firebase.provider';

export interface UserProvider {
  update(user: User): Promise<void>;

  delete(userId: string): Promise<void>;
}

export const getUserService = (): UserProvider => {
  const {cloud} = EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app');

  if (cloud === 'ic') {
    return UserIcProvider.getInstance();
  } else if (cloud === 'firebase') {
    return UserFirebaseProvider.getInstance();
  }

  throw new Error('Not implemented');
};
