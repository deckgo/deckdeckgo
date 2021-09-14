import {User} from '@deckdeckgo/editor';

import store from '../../../stores/user.store';

import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';
import {EnvironmentAppConfig} from '../../../types/core/environment-config';

import {UserIcProvider} from './user.ic.provider';

// TODO remove
import {User as UserModel} from '../../../models/data/user';

export const updateUser = async (user: User) => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  if (!['firebase', 'ic'].includes(cloud)) {
    return;
  }

  // TODO: extract IC
  // TODO: remove type usermodel
  if ('ic' === cloud) {
    await UserIcProvider.getInstance().update(user as UserModel);
    return;
  }

  const cdn: string = 'http://localhost:3335/build/index.esm.js';

  const {updateUser} = await import(cdn);

  const updatedUser: User = await updateUser(user);

  // TODO: remove casting
  store.state.user = {...updatedUser} as UserModel;
};
