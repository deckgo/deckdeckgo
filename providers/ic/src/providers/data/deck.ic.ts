import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Deck, DeckEntries, DeckData, DeleteDeck} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor, Data} from '../../canisters/data/data.did';

import {getIdentity} from '../auth/auth.ic';

import {fromArray, fromTimestamp, toNullable} from '../../utils/did.utils';
import {getDataBucket} from '../../utils/manager.utils';

export const deckEntries: DeckEntries = async (_userId: string): Promise<Deck[]> => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    return [];
  }

  console.log('Deck IC about to request entries');
  const t0 = performance.now();

  const {actor}: {bucket: Principal; actor: DataBucketActor} = await getDataBucket({identity});

  const data: Data[] = await actor.list(toNullable<string>('/decks/'));

  const promises: Promise<Deck>[] = data.map((data: Data) => fromData({data, identity}));
  const decks: Deck[] = await Promise.all(promises);

  const t1 = performance.now();
  console.log(`Deck IC decks done. ${t1 - t0}`, decks);

  return decks;
};

const fromData = async ({data, identity}: {data: Data; identity: Identity}): Promise<Deck> => {
  const deckData: DeckData = await fromArray<DeckData>(data.data);

  return {
    id: data.id,
    data: {
      ...deckData,
      owner_id: identity.getPrincipal().toText(),
      created_at: fromTimestamp(data.created_at),
      updated_at: fromTimestamp(data.updated_at)
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

  console.log('Deck IC about to delete deck and its slides');

  const {actor}: {bucket: Principal; actor: DataBucketActor} = await getDataBucket({identity});

  await actor.del(`/decks/${deckId}`);

  console.log('Deck IC delete');
};
