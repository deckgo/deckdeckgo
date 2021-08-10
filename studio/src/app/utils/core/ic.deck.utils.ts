import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {idlFactory as DecksFactory} from '../../canisters/decks/decks.utils.did';
import {_SERVICE as DecksActor} from '../../canisters/decks/decks.did';

import {idlFactory as DeckBucketFactory} from '../../canisters/deck/deck.utils.did';
import {_SERVICE as DeckBucketActor} from '../../canisters/deck/deck.did';

import {createActor} from './ic.utils';

export const createDecksActor = ({identity}: {identity: Identity}): Promise<DecksActor> => {
  return createActor<DecksActor>({canisterId: process.env.DECKS_CANISTER_ID, idlFactory: DecksFactory, identity});
};

export const createDeckBucketActor = ({identity, bucket}: {identity: Identity; bucket: Principal}): Promise<DeckBucketActor> => {
  return createActor<DeckBucketActor>({canisterId: bucket, idlFactory: DeckBucketFactory, identity});
};
