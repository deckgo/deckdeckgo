import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';
import {DelegationChain, DelegationIdentity, Ed25519KeyIdentity} from '@dfinity/identity';

import {_SERVICE as DecksActor, _SERVICE as DecskActor} from '../canisters/decks/decks.did';
import {_SERVICE as DeckBucketActor} from '../canisters/deck/deck.did';

import {Deck, DeckData} from '../models/data/deck';
import {SyncData, SyncDataDeck} from '../types/editor/sync';

import {createDeckBucketActor, createDecksActor} from '../utils/core/ic.deck.utils';
import {CanisterUtils} from '../utils/editor/canister.utils';

export const uploadWorker = async ({
  delegationChain,
  identityKey,
  syncData,
  host
}: {
  identityKey: string | null;
  delegationChain: string | null;
  syncData: SyncData | undefined;
  host: string;
}) => {
  if (!syncData) {
    return;
  }

  if (!delegationChain || !identityKey) {
    return;
  }

  const chain: DelegationChain = DelegationChain.fromJSON(delegationChain);
  const key: Ed25519KeyIdentity = Ed25519KeyIdentity.fromJSON(identityKey);

  const identity: Identity = DelegationIdentity.fromDelegation(key, chain);

  const {updateDecks} = syncData;

  const decksActor: DecskActor = await createDecksActor({identity, host});

  await uploadDecks({updateDecks, identity, decksActor, host});
};

const uploadDecks = async ({
  updateDecks,
  identity,
  decksActor,
  host
}: {
  updateDecks: SyncDataDeck[] | undefined;
  identity: Identity;
  decksActor: DecskActor;
  host: string;
}) => {
  if (!updateDecks || updateDecks.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) => uploadDeck({deck, decksActor, identity, host}));
  await Promise.all(promises);

  console.log('C synced');
};

const uploadDeck = async ({deck, decksActor, identity, host}: {deck: Deck; decksActor: DecksActor; identity: Identity; host: string}) => {
  if (!deck) {
    return;
  }

  console.log('Deck IC about to SET');
  const t0 = performance.now();

  const bucket: Principal = await decksActor.init(deck.id);

  const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket, host});

  await deckBucket.set({
    deckId: deck.id,
    data: await CanisterUtils.toArray<DeckData>(deck.data),
    created_at: CanisterUtils.toTimestamp((deck.data.created_at as Date) || new Date()),
    updated_at: CanisterUtils.toTimestamp((deck.data.updated_at as Date) || new Date())
  });

  const t1 = performance.now();
  console.log('Deck IC SET done', t1 - t0);

  const t2 = performance.now();

  // TODO: remove, just for test
  console.log('Deck IC Get:', await deckBucket.get(), performance.now() - t2);
};
