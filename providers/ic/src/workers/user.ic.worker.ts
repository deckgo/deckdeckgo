import {Identity} from '@dfinity/agent';

import {User} from '@deckdeckgo/editor';

import {_SERVICE as UserActor, User as UserIc, UserId__1 as UserId, UserSocial as UserSocialIc} from '../canisters/users/users.did';

import {toNullable, toTimestamp, fromNullable, fromUserSocial, fromTimestamp} from '../utils/did.utils';
import {initUserActor} from '../utils/user.utils';
import {initIdentity} from '../utils/identity.utils';

import {InternetIdentityAuth} from '../types/identity';

export const initUserWorker = async ({
  internetIdentity: {delegationChain, identityKey},
  host
}: {
  internetIdentity: InternetIdentityAuth;
  host: string;
}): Promise<User> => {
  if (!delegationChain || !identityKey) {
    return;
  }

  const identity: Identity = initIdentity({identityKey, delegationChain});

  const {userActor, ownerId} = await initUserActor({identity, host});

  console.log('User IC about to GET');
  const t0 = performance.now();

  const user: UserIc | undefined = await get({userActor, ownerId});

  const t1 = performance.now();
  console.log('User IC GET done', t1 - t0, user);

  if (!user) {
    const newUser: UserIc = await createUser({userActor, ownerId});

    return convertUser({user: newUser});
  }

  return convertUser({user});
};

const createUser = async ({userActor, ownerId}: {userActor: UserActor; ownerId: UserId}): Promise<UserIc> => {
  const now: Date = new Date();

  const newUser: UserIc = {
    userId: ownerId,
    data: {
      name: toNullable<string>(null),
      username: toNullable<string>(null),
      bio: toNullable<string>(null),
      photo_url: toNullable<string>(null),
      email: toNullable<string>(null),
      newsletter: [true],
      social: toNullable<UserSocialIc>(null),
      created_at: toTimestamp(now),
      updated_at: toTimestamp(now)
    }
  };

  console.log('User IC about to SET');
  const t0 = performance.now();

  await userActor.set(newUser);

  const t1 = performance.now();
  console.log('User IC SET done', t1 - t0);

  return newUser;
};

const get = async ({userActor, ownerId}: {userActor: UserActor; ownerId: UserId}): Promise<UserIc | undefined> => {
  return fromNullable<UserIc>(await userActor.get(ownerId));
};

const convertUser = ({user}: {user: UserIc}): User => {
  const {userId, data} = user;

  const {name, email, photo_url, newsletter, bio, social, created_at, updated_at, username} = data;

  return {
    id: userId.toText(),
    data: {
      anonymous: false,
      name: fromNullable<string>(name),
      username: fromNullable<string>(username),
      email: fromNullable<string>(email),
      newsletter: fromNullable<boolean>(newsletter),
      photo_url: fromNullable<string>(photo_url),
      social: fromUserSocial<UserSocialIc>(social),
      bio: fromNullable<string>(bio),
      created_at: fromTimestamp(created_at),
      updated_at: fromTimestamp(updated_at)
    }
  };
};
