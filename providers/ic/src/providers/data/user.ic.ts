import {Identity} from '@dfinity/agent';

import {DeleteUser, UpdateUser, User} from '@deckdeckgo/editor';

import {UserSocial as UserSocialIc, User as UserIc} from '../../canisters/users/users.did';

import {getIdentity} from '../auth/auth.ic';

import {initUserActor} from '../../utils/user.utils';
import {toNullable, toTimestamp, toUserSocial} from '../../utils/did.utils';

export const updateUser: UpdateUser = async (user: User): Promise<User> => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    return;
  }

  const {userActor, ownerId} = await initUserActor({identity});

  const {data} = user;

  const {name, email, photo_url, newsletter, bio, social, created_at, username} = data;

  const now: Date = new Date();

  const updateUser: UserIc = {
    userId: ownerId,
    data: {
      name: toNullable<string>(name),
      username: toNullable<string>(username),
      bio: toNullable<string>(bio),
      photo_url: toNullable<string>(photo_url),
      email: toNullable<string>(email),
      newsletter: toNullable<boolean>(newsletter),
      social: toUserSocial<UserSocialIc>(social),
      created_at: toTimestamp(created_at as Date),
      updated_at: toTimestamp(now)
    }
  };

  console.log('User IC about to SET', updateUser);
  const t0 = performance.now();

  await userActor.set(updateUser);

  const t1 = performance.now();
  console.log('User IC SET done', t1 - t0);

  return {
    id: user.id,
    data: {
      ...data,
      updated_at: now
    }
  };
};

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
