import {Identity} from '@dfinity/agent';

import {User} from '@deckdeckgo/editor';

import {UserSocial as UserSocialIc, User as UserIc} from '../../../canisters/users/users.did';

import store from '../../../stores/user.store';

import {CanisterUtils} from '../../../utils/editor/canister.utils';

import {AuthIcProvider} from '../../auth/auth.ic.provider';

import {initUserActor} from '../../../utils/core/ic.user.utils';

export class UserIcProvider {
  private static instance: UserIcProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!UserIcProvider.instance) {
      UserIcProvider.instance = new UserIcProvider();
    }
    return UserIcProvider.instance;
  }

  async update(user: User): Promise<void> {
    const identity: Identity | undefined = AuthIcProvider.getInstance().getIdentity();

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
        name: CanisterUtils.toNullable<string>(name),
        username: CanisterUtils.toNullable<string>(username),
        bio: CanisterUtils.toNullable<string>(bio),
        photo_url: CanisterUtils.toNullable<string>(photo_url),
        email: CanisterUtils.toNullable<string>(email),
        newsletter: CanisterUtils.toNullable<boolean>(newsletter),
        social: CanisterUtils.toUserSocial<UserSocialIc>(social),
        created_at: CanisterUtils.toTimestamp(created_at as Date),
        updated_at: CanisterUtils.toTimestamp(now)
      }
    };

    console.log('User IC about to SET', updateUser);
    const t0 = performance.now();

    await userActor.set(updateUser);

    const t1 = performance.now();
    console.log('User IC SET done', t1 - t0);

    store.state.user = {
      id: user.id,
      data: {
        ...data,
        updated_at: now
      }
    };
  }

  async delete(_userId: string): Promise<void> {
    const identity: Identity | undefined = AuthIcProvider.getInstance().getIdentity();

    if (!identity) {
      return;
    }

    const {userActor, ownerId} = await initUserActor({identity});

    console.log('User IC about to DEL');
    const t0 = performance.now();

    await userActor.del(ownerId);

    const t1 = performance.now();
    console.log('User IC DEL done', t1 - t0);
  }
}
