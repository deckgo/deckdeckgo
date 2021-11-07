import {get} from 'idb-keyval';

import {SyncDataDoc, Doc} from '@deckdeckgo/editor';

import {updateDoc} from '../../providers/data/doc.firebase';

export const uploadDocs = async ({data, userId}: {data: SyncDataDoc[] | undefined; userId: string}): Promise<void> => {
  if (!data || data.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = data.map((doc: SyncDataDoc) => uploadDoc({data: doc, userId}));
  await Promise.all(promises);
};

const uploadDoc = async ({data, userId}: {data: SyncDataDoc; userId: string}): Promise<void> => {
  const {docId}: SyncDataDoc = data;

  const doc: Doc = await get(`/docs/${docId}`);

  if (!doc) {
    return;
  }

  await updateDoc({
    id: docId,
    data: {
      ...doc.data,
      owner_id: userId
    }
  });
};
