import {Identity} from '@dfinity/agent';

import {createActor} from '../../../utils/core/ic.utils';

import {idlFactory as UserFactory} from '../../../canisters/users/users.utils.did';
import {_SERVICE as UserActor, UserSocial as UserSocialIc, User as UserIc, UserId__1 as UserId} from '../../../canisters/users/users.did';
import {CanisterUtils} from '../../../utils/editor/canister.utils';
import authStore from '../../../stores/auth.store';
import {AuthUser} from '../../../models/auth/auth.user';
import userStore from '../../../stores/user.store';

export class UserIcService {
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

  async create({identity}: {identity: Identity}): Promise<void> {
    const userActor: UserActor = await this.createActor({identity});

    const ownerId: UserId = await userActor.getUserId();

    console.log('User IC about to GET');
    const t0 = performance.now();

    const user: UserIc | undefined = await this.get({userActor, ownerId});

    const t1 = performance.now();
    console.log('User IC GET done', t1 - t0, user);

    if (!user) {
      const newUser: UserIc = await this.createUser({userActor, ownerId});

      this.populateUser({user: newUser});

      return;
    }

    this.populateUser({user});
  }

  private async createUser({userActor, ownerId}: {userActor: UserActor; ownerId: UserId}): Promise<UserIc> {
    const now: Date = new Date();

    const newUser: UserIc = {
      userId: ownerId,
      data: {
        bio: CanisterUtils.toNullable<string>(null),
        photo_url: CanisterUtils.toNullable<string>(null),
        social: CanisterUtils.toNullable<UserSocialIc>(null),
        name: CanisterUtils.toNullable<string>(null),
        email: CanisterUtils.toNullable<string>(null),
        newsletter: [true],
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
  }

  private async get({userActor, ownerId}: {userActor: UserActor; ownerId: UserId}): Promise<UserIc | undefined> {
    return CanisterUtils.fromNullable<UserIc>(await userActor.get(ownerId));
  }

  private populateUser({user}: {user: UserIc}) {
    const {userId, data} = user;

    const {name, email, photo_url, newsletter, bio, social, created_at, updated_at} = data;

    authStore.state.authUser = {
      uid: userId.toText(),
      anonymous: false,
      gitHub: false,
      name: CanisterUtils.fromNullable<string>(name),
      email: CanisterUtils.fromNullable<string>(email),
      photo_url: CanisterUtils.fromNullable<string>(photo_url)
    } as AuthUser;

    userStore.state.user = {
      id: userId.toText(),
      data: {
        anonymous: false,
        name: CanisterUtils.fromNullable<string>(name),
        email: CanisterUtils.fromNullable<string>(email),
        newsletter: CanisterUtils.fromNullable<boolean>(newsletter),
        photo_url: CanisterUtils.fromNullable<string>(photo_url),
        social: CanisterUtils.fromUserSocial<UserSocialIc>(social),
        bio: CanisterUtils.fromNullable<string>(bio),
        created_at: CanisterUtils.fromTimestamp(created_at),
        updated_at: CanisterUtils.fromTimestamp(updated_at)
      }
    };
  }

  private createActor({identity}: {identity: Identity}): Promise<UserActor> {
    return createActor<UserActor>({canisterId: process.env.USERS_CANISTER_ID, idlFactory: UserFactory, identity});
  }
}
