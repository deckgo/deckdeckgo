import {User, UpdateUser} from '@deckdeckgo/editor';

import store from '../../../stores/user.store';

import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';
import {EnvironmentAppConfig} from '../../../types/core/environment-config';

import {cloudProvider} from '../../../utils/core/providers.utils';

import {UserIcProvider} from './user.ic.provider';

export const updateUser = async (user: User) => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  if (!['firebase', 'ic'].includes(cloud)) {
    return;
  }

  // TODO: extract IC
  if ('ic' === cloud) {
    await UserIcProvider.getInstance().update(user);
    return;
  }

  const {updateUser}: {updateUser: UpdateUser} = await cloudProvider<{updateUser: UpdateUser}>();

  const updatedUser: User = await updateUser(user);

  store.state.user = {...updatedUser};
};
