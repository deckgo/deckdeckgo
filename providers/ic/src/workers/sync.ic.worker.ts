import {Identity} from '@dfinity/agent';

import {Deck, Doc, Paragraph, Slide, SyncData, SyncDataDeck, SyncDataDoc, SyncDataParagraph, SyncDataSlide} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';

import {InternetIdentityAuth} from '../types/identity';
import {SyncWindow} from '../types/sync.window';
import {SyncStorage, SyncStorageSlide} from '../types/sync.storage';

import {initIdentity} from '../utils/identity.utils';
import {uploadDeckBackgroundAssets, uploadParagraphImages, uploadSlideAssets} from '../utils/sync.storage.utils';
import {uploadDeckData, uploadDocData, uploadParagraphData, uploadSlideData} from '../utils/sync.data.utils';
import {updateDeckBackground, updateParagraphImages, updateSlideChart, updateSlideImages} from '../utils/sync.attributes.utils';
import {BucketActor, getDataBucket} from '../utils/manager.utils';
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
  if (!actor) {
    return;
  }

  // TODO What to do if storage canister is not ready

  await uploadDecks({updateDecks, identity, actor, host, syncWindow});

  await uploadSlides({updateSlides, identity, actor, host, syncWindow});

  await deleteSlides({deleteSlides: slidesToDelete, actor});

  await uploadDocs({updateDocs, actor});

  await uploadParagraphs({updateParagraphs, identity, actor, host, syncWindow});

  await deleteParagraphs({deleteParagraphs: paragraphsToDelete, actor});
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
  host,
  syncWindow
}: {
  updateParagraphs: SyncDataParagraph[] | undefined;
  identity: Identity;
  actor: DataBucketActor;
  host: string;
  syncWindow: SyncWindow;
}) => {
  if (!updateParagraphs || updateParagraphs.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateParagraphs.map(({paragraph, docId}: SyncDataParagraph) =>
    uploadParagraph({paragraph, docId, actor, identity, host, syncWindow})
  );

  await Promise.all(promises);
};

const uploadParagraph = async ({
  paragraph,
  docId,
  actor,
  identity,
  host,
  syncWindow
}: {
  paragraph: Paragraph;
  docId: string;
  actor: DataBucketActor;
  identity: Identity;
  host: string;
  syncWindow: SyncWindow;
}) => {
  if (!paragraph) {
    return;
  }

  // 1. We upload the asset to the IC (worker side), update DOM and IDB (window side for thread safe reason) and clean the asset from IDB
  const images: SyncStorage[] | undefined = await uploadParagraphImages({paragraph, docId, host, identity, syncWindow});

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
