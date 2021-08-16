import {Identity} from '@dfinity/agent';

import {idlFactory as UserFactory} from '../../canisters/users/users.utils.did';
import {_SERVICE as UserActor, UserId__1 as UserId} from '../../canisters/users/users.did';

import {createActor} from './ic.utils';

export const initSlidesActor = async ({identity, host}: {identity: Identity; host?: string}): Promise<{userActor: UserActor; ownerId: UserId}> => {
  const userActor: UserActor = await createSlidesActor({identity, host});

  const ownerId: UserId = await userActor.getUserId();

  return {
    userActor,
    ownerId
  };
};

const createSlidesActor = ({identity, host}: {identity: Identity; host?: string}): Promise<UserActor> => {
  return createActor<UserActor>({canisterId: process.env.USERS_CANISTER_ID, idlFactory: UserFactory, identity, host});
};
