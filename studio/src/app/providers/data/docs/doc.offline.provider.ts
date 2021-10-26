import {set, get} from 'idb-keyval';

import {v4 as uuid} from 'uuid';

import {Doc, DocData} from '@deckdeckgo/editor';

export const createOfflineDoc = (docData: DocData): Promise<Doc> => {
  return new Promise<Doc>(async (resolve, reject) => {
    try {
      const docId: string = uuid();

      const now: Date = new Date();

      const doc: Doc = {
        id: docId,
        data: {
          ...docData,
          updated_at: now,
          created_at: now
        }
      };

      await set(`/docs/${docId}`, doc);

      // TODO: sync update doc
      // await syncUpdateDeck(deckId);

      resolve(doc);
    } catch (err) {
      reject(err);
    }
  });
};

export const getOfflineDoc = (docId: string): Promise<Doc> => {
  return get(`/docs/${docId}`);
};

export const updateOfflineDoc = (doc: Doc): Promise<Doc> => {
  return new Promise<Doc>(async (resolve, reject) => {
    try {
      if (!doc || !doc.data) {
        reject('Invalid doc data');
        return;
      }

      doc.data.updated_at = new Date();

      await set(`/docs/${doc.id}`, doc);

      // TODO: sync
      // await syncUpdateDeck(deck.id);

      resolve(doc);
    } catch (err) {
      reject(err);
    }
  });
};
