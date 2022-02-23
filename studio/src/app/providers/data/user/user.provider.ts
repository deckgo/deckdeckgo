import {UpdateUser, User} from '@deckdeckgo/editor';
import store from '../../../stores/user.store';
import {cloud} from '../../../utils/core/environment.utils';
import {cloudProvider} from '../../../utils/core/providers.utils';

export const updateUser = async (user: User) => {
  if (!cloud()) {
    return;
  }

  const {updateUser}: {updateUser: UpdateUser} = await cloudProvider<{updateUser: UpdateUser}>();

  const updatedUser: User = await updateUser(user);

  store.state.user = {...updatedUser};
};
