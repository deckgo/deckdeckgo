import {Identity} from '@dfinity/agent';

import {Deck, Slide, SyncData, SyncDataDeck, SyncDataSlide} from '@deckdeckgo/editor';

import {_SERVICE as ManagerActor} from '../canisters/manager/manager.did';

import {InternetIdentityAuth} from '../types/identity';
import {SyncWindow} from '../types/sync.window';
import {SyncStorage, SyncStorageSlide} from '../types/sync.storage';

import {createManagerActor} from '../utils/manager.utils';
import {initIdentity} from '../utils/identity.utils';
import {uploadDeckBackgroundAssets, uploadSlideAssets} from '../utils/sync.storage.utils';
import {deleteSlide, uploadDeckData, uploadSlideData} from '../utils/sync.data.utils';
import {updateDeckStorageData, updateSlideStorageData} from '../utils/sync.utils';

export const uploadWorker = async (
  {
    internetIdentity: {delegationChain, identityKey},
    syncData,
    host
  }: {
    internetIdentity: InternetIdentityAuth;
    syncData: SyncData | undefined;
    host: string;
  },
  syncWindow: SyncWindow
) => {
  if (!syncData) {
    return;
  }

  if (!delegationChain || !identityKey) {
    return;
  }

  const identity: Identity = initIdentity({identityKey, delegationChain});

  const {updateDecks, updateSlides, deleteSlides: slidesToDelete} = syncData;

  const managerActor: ManagerActor = await createManagerActor({identity, host});

  await uploadDecks({updateDecks, identity, managerActor, host, syncWindow});

  await uploadSlides({updateSlides, identity, managerActor, host, syncWindow});

  await deleteSlides({deleteSlides: slidesToDelete, identity, managerActor, host});

  // TODO: handle delete decks here?

  // TODO: upload slides assets and update DOM through postMessage
};

const uploadDecks = async ({
  updateDecks,
  identity,
  managerActor,
  host,
  syncWindow
}: {
  updateDecks: SyncDataDeck[] | undefined;
  identity: Identity;
  managerActor: ManagerActor;
  host: string;
  syncWindow: SyncWindow;
}) => {
  if (!updateDecks || updateDecks.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) =>
    uploadDeck({deck, managerActor: managerActor, identity, host, syncWindow})
  );
  await Promise.all(promises);

  console.log('Deck IC synced');
};

const uploadDeck = async ({
  deck,
  managerActor,
  identity,
  host,
  syncWindow
}: {
  deck: Deck;
  managerActor: ManagerActor;
  identity: Identity;
  host: string;
  syncWindow: SyncWindow;
}) => {
  if (!deck) {
    return;
  }

  // 1. We upload the asset to the IC (worker side), update DOM and IDB (window side for thread safe reason) and clean the asset from IDB
  const {src: imgSrc, storageFile}: SyncStorage = await uploadDeckBackgroundAssets({deck, host, identity, syncWindow});

  // 2. If we uploaded an asset, its URL has changed (no more local but available online)
  const updateDeck: Deck = updateDeckStorageData({deck, imgSrc, storageFile});

  // 3. We can update the data in the IC
  await uploadDeckData({deck: updateDeck, managerActor, identity, host});
};

const uploadSlides = async ({
  updateSlides,
  identity,
  managerActor,
  host,
  syncWindow
}: {
  updateSlides: SyncDataSlide[] | undefined;
  identity: Identity;
  managerActor: ManagerActor;
  host: string;
  syncWindow: SyncWindow;
}) => {
  if (!updateSlides || updateSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateSlides.map(({slide, deckId}: SyncDataSlide) =>
    uploadSlide({slide, deckId, managerActor: managerActor, identity, host, syncWindow})
  );

  await Promise.all(promises);
};

const deleteSlides = async ({
  deleteSlides,
  identity,
  managerActor,
  host
}: {
  deleteSlides: SyncDataSlide[] | undefined;
  identity: Identity;
  managerActor: ManagerActor;
  host: string;
}) => {
  if (!deleteSlides || deleteSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = deleteSlides.map(({deckId, slideId}: SyncDataSlide) =>
    deleteSlide({slideId, deckId, identity, managerActor: managerActor, host})
  );

  await Promise.all(promises);
};

const uploadSlide = async ({
  slide,
  deckId,
  managerActor,
  identity,
  host,
  syncWindow
}: {
  slide: Slide;
  deckId: string;
  managerActor: ManagerActor;
  identity: Identity;
  host: string;
  syncWindow: SyncWindow;
}) => {
  if (!slide) {
    return;
  }

  // 1. We upload the asset to the IC (worker side), update DOM and IDB (window side for thread safe reason) and clean the asset from IDB
  const {images, chart}: SyncStorageSlide = await uploadSlideAssets({slide, deckId, host, identity, syncWindow});

  // 2. If we uploaded assets, there URL have changed (no more local but available online)
  const updateSlide: Slide = updateSlideStorageData({slide, images, chart});

  // 3. We can update the data in the IC
  await uploadSlideData({slide: updateSlide, deckId, managerActor, identity, host});
};
