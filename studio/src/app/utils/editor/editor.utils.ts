import {del, get, set} from 'idb-keyval';

import {Editor} from '../../types/editor/editor';

import {clearSync} from '../../providers/sync/sync.provider';

import {ImageHistoryService} from '../../services/editor/image-history/image-history.service';

export const getEdit = (): Promise<Editor | undefined> => get('deckdeckgo_editor');

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
