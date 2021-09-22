import {DeleteUser} from '@deckdeckgo/editor';
import {Identity} from '@dfinity/agent';

import {initUserActor} from '../../utils/user.utils';

import {getIdentity} from '../auth/auth.ic';

export const deleteUser: DeleteUser = async (_userId: string): Promise<void> => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    return;
  }

  const {userActor, ownerId} = await initUserActor({identity});

  console.log('User IC about to DEL');
  const t0 = performance.now();

  await userActor.del(ownerId);

  const t1 = performance.now();
  console.log('User IC DEL done', t1 - t0);
};
