import {Identity} from '@dfinity/agent';

import {UserSocial as UserSocialIc, User as UserIc} from '../../../canisters/users/users.did';

import store from '../../../stores/user.store';

import {CanisterUtils} from '../../../utils/editor/canister.utils';

import {User} from '../../../models/data/user';

import {AuthFactoryService} from '../../auth/auth.factory.service';
import {UserService} from './user.service';
import {AuthIcService} from '../../auth/auth.ic.service';

import {initSlidesActor} from '../../../utils/core/ic.slide.utils';

export class UserIcService implements UserService {
  private static instance: UserIcService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!UserIcService.instance) {
      UserIcService.instance = new UserIcService();
    }
    return UserIcService.instance;
  }

  // @Override
  async update(user: User): Promise<void> {
    const identity: Identity | undefined = (AuthFactoryService.getInstance() as AuthIcService).getIdentity();

    if (!identity) {
      return;
    }

    const {userActor, ownerId} = await initSlidesActor({identity});

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

  // @Override
  async delete(_userId: string): Promise<void> {
    const identity: Identity | undefined = (AuthFactoryService.getInstance() as AuthIcService).getIdentity();

    if (!identity) {
      return;
    }

    const {userActor, ownerId} = await initSlidesActor({identity});

    console.log('User IC about to DEL');
    const t0 = performance.now();

    await userActor.del(ownerId);

    const t1 = performance.now();
    console.log('User IC DEL done', t1 - t0);
  }
}
