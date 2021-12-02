import {Identity} from '@dfinity/agent';

import {Deck, Doc, Paragraph, Slide, SyncData, SyncDataDeck, SyncDataDoc, SyncDataParagraph, SyncDataSlide} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';
import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';

import {InternetIdentityAuth} from '../types/identity';
import {SyncWindow} from '../types/sync.window';
import {SyncStorage, SyncStorageSlide} from '../types/sync.storage';

import {initIdentity} from '../utils/identity.utils';
import {uploadDeckBackgroundAssets, uploadParagraphImages, uploadSlideAssets} from '../utils/sync.storage.utils';
import {uploadDeckData, uploadDocData, uploadParagraphData, uploadSlideData} from '../utils/sync.data.utils';
import {updateDeckBackground, updateParagraphImages, updateSlideChart, updateSlideImages} from '../utils/sync.attributes.utils';
import {BucketActor, getDataBucket, getStorageBucket} from '../utils/manager.utils';
import {deleteData} from '../utils/data.utils';

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

  const {
    updateDecks,
    updateDocs,
    updateSlides,
    updateParagraphs,
    deleteSlides: slidesToDelete,
    deleteParagraphs: paragraphsToDelete
  } = syncData;

  // For performance reason, we query the actor only once. We might not need it but, most often we will and multiple times.
  const {actor}: BucketActor<DataBucketActor> = await getDataBucket({host, identity});

  // In case the bucket does not yet exist, do not sync yet and wait for next poll
  // Can most probably not happens since user auth create such bucket and triggers the sync when ready
  if (!actor) {
    return;
  }

  // We query the storage actor until it is initialized and use it to upload the assets
  const storageBucket: BucketActor<StorageBucketActor> = await getStorageBucket({host, identity});

  const {actor: storageActor} = storageBucket;

  if (!storageActor) {
    return;
  }

  await uploadDecks({updateDecks, identity, dataActor: actor, storageBucket, syncWindow});

  await uploadSlides({updateSlides, identity, actor, storageBucket, syncWindow});

  await deleteSlides({deleteSlides: slidesToDelete, actor});

  await uploadDocs({updateDocs, actor});

  await uploadParagraphs({updateParagraphs, identity, actor, storageBucket, syncWindow});

  await deleteParagraphs({deleteParagraphs: paragraphsToDelete, actor});
};

const uploadDecks = async ({
  updateDecks,
  identity,
  dataActor,
  storageBucket,
  syncWindow
}: {
  updateDecks: SyncDataDeck[] | undefined;
  identity: Identity;
  dataActor: DataBucketActor;
  storageBucket: BucketActor<StorageBucketActor>;
  syncWindow: SyncWindow;
}) => {
  if (!updateDecks || updateDecks.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) =>
    uploadDeck({deck, dataActor, storageBucket, identity, syncWindow})
  );
  await Promise.all(promises);

  console.log('Deck IC synced');
};

const uploadDeck = async ({
  deck,
  dataActor,
  storageBucket,
  identity,
  syncWindow
}: {
  deck: Deck;
  dataActor: DataBucketActor;
  storageBucket: BucketActor<StorageBucketActor>;
  identity: Identity;
  syncWindow: SyncWindow;
}) => {
  if (!deck) {
    return;
  }

  // 1. We upload the asset to the IC (worker side), update DOM and IDB (window side for thread safe reason) and clean the asset from IDB
  const {src: imgSrc, storageFile}: SyncStorage = await uploadDeckBackgroundAssets({deck, identity, syncWindow, storageBucket});

  // 2. If we uploaded an asset, its URL has changed (no more local but available online)
  const updateDeck: Deck = updateDeckBackground({deck, imgSrc, storageFile});

  // 3. We can update the data in the IC
  await uploadDeckData({deck: updateDeck, actor: dataActor});
};

const uploadSlides = async ({
  updateSlides,
  identity,
  actor,
  syncWindow,
  storageBucket
}: {
  updateSlides: SyncDataSlide[] | undefined;
  identity: Identity;
  actor: DataBucketActor;
  syncWindow: SyncWindow;
  storageBucket: BucketActor<StorageBucketActor>;
}) => {
  if (!updateSlides || updateSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateSlides.map(({slide, deckId}: SyncDataSlide) =>
    uploadSlide({slide, deckId, actor, identity, storageBucket, syncWindow})
  );

  await Promise.all(promises);
};

const deleteSlides = async ({deleteSlides, actor}: {deleteSlides: SyncDataSlide[] | undefined; actor: DataBucketActor}) => {
  if (!deleteSlides || deleteSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = deleteSlides
    .filter(({slideId}: SyncDataSlide) => slideId !== undefined)
    .map(({deckId, slideId}: SyncDataSlide) => deleteData({key: `/decks/${deckId}/slides/${slideId}`, actor}));

  await Promise.all(promises);
};

const uploadSlide = async ({
  slide,
  deckId,
  actor,
  identity,
  syncWindow,
  storageBucket
}: {
  slide: Slide;
  deckId: string;
  actor: DataBucketActor;
  identity: Identity;
  syncWindow: SyncWindow;
  storageBucket: BucketActor<StorageBucketActor>;
}) => {
  if (!slide) {
    return;
  }

  // 1. We upload the asset to the IC (worker side), update DOM and IDB (window side for thread safe reason) and clean the asset from IDB
  const {images, chart}: SyncStorageSlide = await uploadSlideAssets({slide, deckId, storageBucket, identity, syncWindow});

  // 2. If we uploaded assets, there URL have changed (no more local but available online)
  const updateChartSlide: Slide = updateSlideChart({slide, chart});
  const updateSlide: Slide = updateSlideImages({slide: updateChartSlide, images});

  // 3. We can update the data in the IC
  await uploadSlideData({deckId, slide: updateSlide, actor});

  console.log(`Slide (${updateSlide.id}) IC synced`);
};

const uploadDocs = async ({updateDocs, actor}: {updateDocs: SyncDataDoc[] | undefined; actor: DataBucketActor}) => {
  if (!updateDocs || updateDocs.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateDocs.map(({doc}: SyncDataDoc) => uploadDoc({doc, actor}));
  await Promise.all(promises);

  console.log('Doc IC synced');
};

const uploadDoc = async ({doc, actor}: {doc: Doc; actor: DataBucketActor}) => {
  if (!doc) {
    return;
  }

  await uploadDocData({doc, actor});
};

const uploadParagraphs = async ({
  updateParagraphs,
  identity,
  actor,
  syncWindow,
  storageBucket
}: {
  updateParagraphs: SyncDataParagraph[] | undefined;
  identity: Identity;
  actor: DataBucketActor;
  syncWindow: SyncWindow;
  storageBucket: BucketActor<StorageBucketActor>;
}) => {
  if (!updateParagraphs || updateParagraphs.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateParagraphs.map(({paragraph, docId}: SyncDataParagraph) =>
    uploadParagraph({paragraph, docId, actor, identity, storageBucket, syncWindow})
  );

  await Promise.all(promises);
};

const uploadParagraph = async ({
  paragraph,
  docId,
  actor,
  identity,
  syncWindow,
  storageBucket
}: {
  paragraph: Paragraph;
  docId: string;
  actor: DataBucketActor;
  identity: Identity;
  syncWindow: SyncWindow;
  storageBucket: BucketActor<StorageBucketActor>;
}) => {
  if (!paragraph) {
    return;
  }

  // 1. We upload the asset to the IC (worker side), update DOM and IDB (window side for thread safe reason) and clean the asset from IDB
  const images: SyncStorage[] | undefined = await uploadParagraphImages({paragraph, docId, storageBucket, identity, syncWindow});

  // 2. If we uploaded assets, there URL have changed (no more local but available online)
  const updateParagraph: Paragraph = updateParagraphImages({paragraph: paragraph, images});

  // 3. We can update the data in the IC
  await uploadParagraphData({docId, paragraph: updateParagraph, actor});

  console.log(`Paragraph (${updateParagraph.id}) IC synced`);
};

const deleteParagraphs = async ({deleteParagraphs, actor}: {deleteParagraphs: SyncDataParagraph[] | undefined; actor: DataBucketActor}) => {
  if (!deleteParagraphs || deleteParagraphs.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = deleteParagraphs
    .filter(({paragraphId}: SyncDataParagraph) => paragraphId !== undefined)
    .map(({docId, paragraphId}: SyncDataParagraph) => deleteData({key: `/docs/${docId}/paragraphs/${paragraphId}`, actor}));

  await Promise.all(promises);
};
