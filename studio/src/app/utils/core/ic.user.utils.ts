import {Identity} from '@dfinity/agent';

import {idlFactory as UserFactory} from '../../canisters/users/users.utils.did';
import {_SERVICE as UserActor, UserId__1 as UserId} from '../../canisters/users/users.did';

import {createActor} from './ic.utils';

// TODO: remove

export const initUserActor = async ({
  identity,
  host
}: {
  identity: Identity;
  host?: string;
}): Promise<{userActor: UserActor; ownerId: UserId}> => {
  const userActor: UserActor = await createUserActor({identity, host});

  const ownerId: UserId = await userActor.getUserId();

  return {
    userActor,
    ownerId
  };
};

const createUserActor = ({identity, host}: {identity: Identity; host?: string}): Promise<UserActor> => {
  return createActor<UserActor>({canisterId: process.env.USERS_CANISTER_ID, idlFactory: UserFactory, identity, host});
};
