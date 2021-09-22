import {User, UpdateUser} from '@deckdeckgo/editor';

import store from '../../../stores/user.store';

import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';
import {EnvironmentAppConfig} from '../../../types/core/environment-config';

import {cloudProvider} from '../../../utils/core/providers.utils';

export const updateUser = async (user: User) => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  if (!['firebase', 'ic'].includes(cloud)) {
    return;
  }

  const {updateUser}: {updateUser: UpdateUser} = await cloudProvider<{updateUser: UpdateUser}>();

  const updatedUser: User = await updateUser(user);

  store.state.user = {...updatedUser};
};
