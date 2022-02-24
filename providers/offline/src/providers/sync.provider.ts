import {del, delMany, keys} from 'idb-keyval';

export const clearSync = async () => {
  await del('deckdeckgo_pending_sync');

  const storageKeys: string[] = (await keys<string>()).filter(
    (key: string) => key.startsWith('/decks/') || key.startsWith('/docs/') || key.startsWith('/assets/')
  );

  if (!storageKeys.length) {
    return;
  }

  await delMany(storageKeys);
};
