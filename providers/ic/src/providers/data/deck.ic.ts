import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Deck, DeckEntries, DeckData, DeleteDeck} from '@deckdeckgo/editor';

import {_SERVICE as ManagerActor} from '../../canisters/manager/manager.did';

import {_SERVICE as DeckBucketActor, Deck as DeckIc} from '../../canisters/deck/deck.did';

import {getIdentity} from '../auth/auth.ic';

import {createDeckBucketActor, createManagerActor} from '../../utils/manager.utils';
import {fromArray, fromTimestamp} from '../../utils/did.utils';

export const deckEntries: DeckEntries = async (_userId: string): Promise<Deck[]> => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    return [];
  }

  const managerActor: ManagerActor = await createManagerActor({identity});

  console.log('Deck IC about to request entries');

  const buckets: Principal[] = await managerActor.deckEntries();

  console.log('Deck IC entries done.', buckets);

  const promises: Promise<DeckIc | undefined>[] = buckets.map((bucket: Principal) => getDeckIc({bucket, identity}));

  const decksIc: DeckIc[] = await Promise.all(promises);

  console.log('Deck IC decks done.', decksIc);

  const decksPromises: Promise<Deck>[] = decksIc
    ?.filter((deck: DeckIc | undefined) => deck !== undefined)
    .map((deck: DeckIc) => fromDeck({deck, identity}));
  const decks: Deck[] = await Promise.all(decksPromises);

  return decks;
};

const getDeckIc = async ({bucket, identity}: {bucket: Principal; identity: Identity}): Promise<DeckIc | undefined> => {
  try {
    const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket});

    return await deckBucket.get();
  } catch (err) {
    console.error('Deck cannot be found.', bucket);
    return undefined;
  }
};

const fromDeck = async ({deck, identity}: {deck: DeckIc; identity: Identity}): Promise<Deck> => {
  const data: DeckData = await fromArray<DeckData>(deck.data);

  return {
    id: deck.deckId,
    data: {
      ...data,
      owner_id: identity.getPrincipal().toText(),
      created_at: fromTimestamp(deck.created_at),
      updated_at: fromTimestamp(deck.updated_at)
    }
  };
};

export const deleteDeck: DeleteDeck = async (deckId: string): Promise<void> => {
  if (!deckId) {
    return;
  }

  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    return;
  }

  // We delete the all canister of the related deckId

  const managerActor: ManagerActor = await createManagerActor({identity});

  console.log('Deck IC about to delete deck and its slides');

  await managerActor.delDeck(deckId);

  console.log('Deck IC delete');
};
