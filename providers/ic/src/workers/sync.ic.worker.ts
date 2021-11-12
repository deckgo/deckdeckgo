import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Deck, Slide, SyncData, SyncDataDeck, SyncDataSlide} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';

import {InternetIdentityAuth} from '../types/identity';
import {SyncWindow} from '../types/sync.window';
import {SyncStorage, SyncStorageSlide} from '../types/sync.storage';

import {initIdentity} from '../utils/identity.utils';
import {uploadDeckBackgroundAssets, uploadSlideAssets} from '../utils/sync.storage.utils';
import {deleteSlide, uploadDeckData, uploadSlideData} from '../utils/sync.data.utils';
import {updateDeckBackground, updateSlideChart, updateSlideImages} from '../utils/sync.attributes.utils';
import {getDataBucket} from '../utils/manager.utils';

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

  // For performance reason, we query the actor only once. We might not need it but, most often we will and multiple times.
  const {actor}: {bucket: Principal; actor: DataBucketActor} = await getDataBucket({host, identity});

  await uploadDecks({updateDecks, identity, actor, host, syncWindow});

  await uploadSlides({updateSlides, identity, actor, host, syncWindow});

  await deleteSlides({deleteSlides: slidesToDelete, actor});
};

const uploadDecks = async ({
  updateDecks,
  identity,
  actor,
  host,
  syncWindow
}: {
  updateDecks: SyncDataDeck[] | undefined;
  identity: Identity;
  actor: DataBucketActor;
  host: string;
  syncWindow: SyncWindow;
}) => {
  if (!updateDecks || updateDecks.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) => uploadDeck({deck, actor, identity, host, syncWindow}));
  await Promise.all(promises);

  console.log('Deck IC synced');
};

const uploadDeck = async ({
  deck,
  actor,
  identity,
  host,
  syncWindow
}: {
  deck: Deck;
  actor: DataBucketActor;
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
  const updateDeck: Deck = updateDeckBackground({deck, imgSrc, storageFile});

  // 3. We can update the data in the IC
  await uploadDeckData({deck: updateDeck, actor});
};

const uploadSlides = async ({
  updateSlides,
  identity,
  actor,
  host,
  syncWindow
}: {
  updateSlides: SyncDataSlide[] | undefined;
  identity: Identity;
  actor: DataBucketActor;
  host: string;
  syncWindow: SyncWindow;
}) => {
  if (!updateSlides || updateSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateSlides.map(({slide, deckId}: SyncDataSlide) =>
    uploadSlide({slide, deckId, actor, identity, host, syncWindow})
  );

  await Promise.all(promises);
};

const deleteSlides = async ({deleteSlides, actor}: {deleteSlides: SyncDataSlide[] | undefined; actor: DataBucketActor}) => {
  if (!deleteSlides || deleteSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = deleteSlides.map(({deckId, slideId}: SyncDataSlide) => deleteSlide({slideId, deckId, actor}));

  await Promise.all(promises);
};

const uploadSlide = async ({
  slide,
  deckId,
  actor,
  identity,
  host,
  syncWindow
}: {
  slide: Slide;
  deckId: string;
  actor: DataBucketActor;
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
  const updateChartSlide: Slide = updateSlideChart({slide, chart});
  const updateSlide: Slide = updateSlideImages({slide: updateChartSlide, images});

  // 3. We can update the data in the IC
  await uploadSlideData({slide: updateSlide, deckId, actor});
};
