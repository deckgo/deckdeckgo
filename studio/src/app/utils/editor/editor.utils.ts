import {del} from 'idb-keyval';

import {clearSync} from '../../providers/sync/sync.provider';

import {ImageHistoryService} from '../../services/editor/image-history/image-history.service';

export const clearEdit = async (clearSyncData: boolean) => {
  if (clearSyncData) {
    await clearSync();
  }

  await ImageHistoryService.getInstance().clear();

  // By removing the reference to the current deck in indexeddb, it will create a new deck on reload
  await del('deckdeckgo_deck_id');
};
