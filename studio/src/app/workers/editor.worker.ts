import {get, getMany, keys} from 'idb-keyval';

import {Slide} from '../models/data/slide';
import {Deck} from '../models/data/deck';
import {SyncData} from '../types/editor/sync-data';

// TODO: move Firestore merge to worker

export const syncTimer = async () => {
  // TODO: clean interval

  setInterval(async () => {
    const data: SyncData | undefined = await syncData();

    // @ts-ignore
    postMessage(data);
  }, 5000);
};

const syncData = async (): Promise<SyncData | undefined> => {
  const deckId: string | undefined = await get<string>('deckdeckgo_deck_id');

  if (!deckId) {
    return undefined;
  }

  const deck: Deck | undefined = await get(`/decks/${deckId}`);

  const slideIds: string[] | undefined = await keys();

  const slideKeys: string[] = slideIds?.filter((key: string) => key.indexOf(`/decks/${deckId}/slides/`) > -1);

  const slides: Slide[] | undefined = await getMany(slideKeys);

  // TODO: Filter on updated_at

  return {
    deckId,
    deck,
    slides
  };
};
