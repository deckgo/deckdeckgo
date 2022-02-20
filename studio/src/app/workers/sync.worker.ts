import {SyncData} from '@deckdeckgo/editor';
import {collectData} from '@deckdeckgo/studio';

// TODO: move Firestore merge to worker

let timer: NodeJS.Timeout = undefined;

export const startSyncTimer = async () => {
  timer = setInterval(async () => await syncData(), 5000);
};

export const stopSyncTimer = async () => {
  if (!timer) {
    return;
  }

  await syncData();

  clearInterval(timer);
  timer = undefined;
};

// TODO: there is probably a way to refactor these functions with the help of generic or the abstract interface...

const syncData = async () => {
  // TODO: Avoid atomic errors, window updating while worker running. If we can move Firestore and ICP to the worker it solves everything though.

  const data: SyncData | undefined = await collectData();

  // Do not stress window side if there are no data to sync
  if (!data) {
    return;
  }

  const {updateDecks, deleteDecks, deleteSlides, updateSlides, updateDocs, deleteDocs, deleteParagraphs, updateParagraphs} = data;

  if (
    updateDecks?.length === 0 &&
    deleteDecks?.length === 0 &&
    deleteSlides?.length === 0 &&
    updateSlides?.length === 0 &&
    updateDocs?.length === 0 &&
    deleteDocs?.length === 0 &&
    deleteParagraphs?.length === 0 &&
    updateParagraphs?.length === 0
  ) {
    return;
  }

  // @ts-ignore
  postMessage({
    msg: 'deckdeckgo_sync',
    data
  });
};
