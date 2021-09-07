import {Identity} from '@dfinity/agent';

import {_SERVICE as UserActor, User as UserIc, UserId__1 as UserId, UserSocial as UserSocialIc} from '../canisters/users/users.did';

import {User} from '../models/data/user';

import {CanisterUtils} from '../utils/editor/canister.utils';
import {initSlidesActor} from '../utils/core/ic.slide.utils';
import {initIdentity} from '../utils/core/ic.identity.utils';

import {InternetIdentityAuth} from '../types/core/ic.identity';

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

  const {userActor, ownerId} = await initSlidesActor({identity, host});

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
      name: CanisterUtils.toNullable<string>(null),
      username: CanisterUtils.toNullable<string>(null),
      bio: CanisterUtils.toNullable<string>(null),
      photo_url: CanisterUtils.toNullable<string>(null),
      email: CanisterUtils.toNullable<string>(null),
      newsletter: [true],
      social: CanisterUtils.toNullable<UserSocialIc>(null),
      created_at: CanisterUtils.toTimestamp(now),
      updated_at: CanisterUtils.toTimestamp(now)
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
  return CanisterUtils.fromNullable<UserIc>(await userActor.get(ownerId));
};

const convertUser = ({user}: {user: UserIc}): User => {
  const {userId, data} = user;

  const {name, email, photo_url, newsletter, bio, social, created_at, updated_at, username} = data;

  return {
    id: userId.toText(),
    data: {
      anonymous: false,
      name: CanisterUtils.fromNullable<string>(name),
      username: CanisterUtils.fromNullable<string>(username),
      email: CanisterUtils.fromNullable<string>(email),
      newsletter: CanisterUtils.fromNullable<boolean>(newsletter),
      photo_url: CanisterUtils.fromNullable<string>(photo_url),
      social: CanisterUtils.fromUserSocial<UserSocialIc>(social),
      bio: CanisterUtils.fromNullable<string>(bio),
      created_at: CanisterUtils.fromTimestamp(created_at),
      updated_at: CanisterUtils.fromTimestamp(updated_at)
    }
  };
};
