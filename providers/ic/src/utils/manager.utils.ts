import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {idlFactory as ManagerFactory} from '../canisters/manager/manager.utils.did';
import {_SERVICE as ManagerActor} from '../canisters/manager/manager.did';

import {idlFactory as DeckBucketFactory} from '../canisters/deck/deck.utils.did';
import {_SERVICE as DeckBucketActor} from '../canisters/deck/deck.did';

import {idlFactory as StorageBucketFactory} from '../canisters/storage/storage.utils.did';
import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';

import {createActor} from './actor.utils';
import {fromNullable} from './did.utils';

export const createManagerActor = ({identity, host}: {identity: Identity; host?: string}): Promise<ManagerActor> => {
  return createActor<ManagerActor>({canisterId: process.env.MANAGER_CANISTER_ID, idlFactory: ManagerFactory, identity, host});
};

export const createDeckBucketActor = ({
  identity,
  bucket,
  host
}: {
  identity: Identity;
  bucket: Principal;
  host?: string;
}): Promise<DeckBucketActor> => {
  return createActor<DeckBucketActor>({canisterId: bucket, idlFactory: DeckBucketFactory, identity, host});
};

export const createStorageBucketActor = ({
  identity,
  bucket,
  host
}: {
  identity: Identity;
  bucket: Principal;
  host?: string;
}): Promise<StorageBucketActor> => {
  return createActor<StorageBucketActor>({canisterId: bucket, idlFactory: StorageBucketFactory, identity, host});
};

export const initDeckBucket = async ({managerActor, deckId}: {managerActor: ManagerActor; deckId: string}): Promise<Principal> => {
  const existingBucket: Principal | undefined = fromNullable<Principal>(await managerActor.getDeck(deckId));

  if (!existingBucket) {
    return await managerActor.initDeck(deckId);
  }

  return existingBucket;
};

export const initStorageBucket = async ({managerActor}: {managerActor: ManagerActor}): Promise<Principal> => {
  const existingBucket: Principal | undefined = fromNullable<Principal>(await managerActor.getStorage());

  if (!existingBucket) {
    return await managerActor.initStorage();
  }

  return existingBucket;
};
