import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {idlFactory as ManagerFactory} from '../../canisters/manager/manager.utils.did';
import {_SERVICE as ManagerActor} from '../../canisters/manager/manager.did';

import {idlFactory as DeckBucketFactory} from '../../canisters/deck/deck.utils.did';
import {_SERVICE as DeckBucketActor} from '../../canisters/deck/deck.did';

import {createActor} from './ic.utils';

export const createManagerActor = ({identity, host}: {identity: Identity; host?: string}): Promise<ManagerActor> => {
  return createActor<ManagerActor>({canisterId: process.env.MANAGER_CANISTER_ID, idlFactory: ManagerFactory, identity, host});
};

export const createDeckBucketActor = ({identity, bucket, host}: {identity: Identity; bucket: Principal; host?: string}): Promise<DeckBucketActor> => {
  return createActor<DeckBucketActor>({canisterId: bucket, idlFactory: DeckBucketFactory, identity, host});
};
