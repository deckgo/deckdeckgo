import {UpdateUser, User} from '@deckdeckgo/editor';
import {EnvStore} from '../stores/env.store';
import {UserStore} from '../stores/user.store';
import {cloudProvider} from '../utils/providers.utils';

export const updateUser = async (user: User) => {
  if (!EnvStore.getInstance().cloud()) {
    return;
  }

  const {updateUser}: {updateUser: UpdateUser} = await cloudProvider<{updateUser: UpdateUser}>();

  const updatedUser: User = await updateUser(user);

  UserStore.getInstance().set({...updatedUser});
};
