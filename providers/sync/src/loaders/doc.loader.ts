import {Doc} from '@deckdeckgo/editor';
import {getOfflineDoc} from '@deckdeckgo/offline';
import {DocStore} from '../stores/doc.store';

export const loadDoc = async (docId: string | undefined): Promise<Doc> => {
  if (!docId) {
    DocStore.getInstance().set(null);
    throw new Error('Doc is not defined');
  }

  const doc: Doc = await getOfflineDoc(docId);

  if (!doc || !doc.data) {
    DocStore.getInstance().set(null);
    throw new Error('No doc could be fetched');
  }

  DocStore.getInstance().set({...doc});

  return doc;
};

export const resetDoc = () => DocStore.getInstance().set(null);
