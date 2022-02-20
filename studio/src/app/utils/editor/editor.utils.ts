import {clearSync} from '@deckdeckgo/studio';
import {del, get, set} from 'idb-keyval';
import {ImageHistoryService} from '../../services/editor/image-history/image-history.service';
import {Editor} from '../../types/editor/editor';

export const getEdit = async (): Promise<Editor | undefined> => {
  let edit: Editor | undefined = await get('deckdeckgo_editor');

  if (!edit) {
    edit = await migrateDeckId();
  }

  return edit;
};

// TODO: remove after a while, key used to be 'deckdeckgo_deck_id' and is now 'deckdeckgo_editor'
const migrateDeckId = async (): Promise<Editor | undefined> => {
  const deckId: string | undefined = await get('deckdeckgo_deck_id');

  if (!deckId) {
    return undefined;
  }

  await setEditDeckId(deckId);

  await del('deckdeckgo_deck_id');

  return {
    id: deckId,
    type: 'deck'
  };
};

export const clearEdit = async (clearSyncData: boolean) => {
  if (clearSyncData) {
    await clearSync();
  }

  await ImageHistoryService.getInstance().clear();

  // By removing the reference to the current edited deck or deck in indexeddb, it will create a new deck on reload
  await del('deckdeckgo_editor');
};

export const setEditDeckId = (id: string): Promise<void> => set('deckdeckgo_editor', {id, type: 'deck'});

export const setEditDocId = (id: string): Promise<void> => set('deckdeckgo_editor', {id, type: 'doc'});
