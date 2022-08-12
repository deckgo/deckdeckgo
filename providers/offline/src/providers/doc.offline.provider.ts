import type {Doc, DocData} from '@deckdeckgo/editor';
import {get, set, update} from 'idb-keyval';
import {nanoid} from 'nanoid';

export const createOfflineDoc = async (docData: DocData): Promise<Doc> => {
  const docId: string = nanoid();

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

  return doc;
};

export const getOfflineDoc = (docId: string): Promise<Doc | undefined> => get(`/docs/${docId}`);

export const updateOfflineDoc = (doc: Doc): Promise<void> =>
  update(`/docs/${doc.id}`, ({data}: Doc) => ({
    id: doc.id,
    data: {
      ...data,
      ...doc.data
    }
  }));
