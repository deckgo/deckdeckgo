import {Deck, Doc, Paragraph, Slide, SyncData, SyncDataDeck, SyncDataDoc, SyncDataParagraph, SyncDataSlide} from '@deckdeckgo/editor';
import {Identity} from '@dfinity/agent';
import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';
import {_SERVICE as StorageBucketActor} from '../canisters/storage/storage.did';
import {InternetIdentityAuth} from '../types/identity';
import {SyncStorage, SyncStorageSlide} from '../types/sync.storage';
import {LogWindow, SyncWindow} from '../types/sync.window';
import {deleteData} from '../utils/data.utils';
import {initIdentity} from '../utils/identity.utils';
import {BucketActor, getDataBucket, getStorageBucket} from '../utils/manager.utils';
import {updateDeckBackground, updateParagraphImages, updateSlideChart, updateSlideImages} from '../utils/sync.attributes.utils';
import {uploadDeckData, uploadDocData, uploadParagraphData, uploadSlideData} from '../utils/sync.data.utils';
import {uploadDeckBackgroundAssets, uploadParagraphImages, uploadSlideAssets} from '../utils/sync.storage.utils';

export const uploadWorker = async (
  {
    internetIdentity: {delegationChain, identityKey},
    syncData
  }: {
    internetIdentity: InternetIdentityAuth;
    syncData: SyncData | undefined;
  },
  syncWindow: SyncWindow,
  log: LogWindow
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

  const promises: [Promise<BucketActor<DataBucketActor>>, Promise<BucketActor<StorageBucketActor>>] = [
    getDataBucket({identity}),
    getStorageBucket({identity})
  ];

  const [dataBucket, storageBucket] = await Promise.all(promises);

  // For performance reason, we query the actor only once. We might not need it but, most often we will and multiple times.
  const {actor}: BucketActor<DataBucketActor> = dataBucket;

  // In case the bucket does not yet exist, do not sync yet and wait for next poll
  // Can most probably not happens since user auth create such bucket and triggers the sync when ready
  if (!actor) {
    return;
  }

  // TODO: what's the impact in term of performance to query it every time? indeed we need to init it the very first time to initialize the bucket but, we might not need it to sync data most of the time?
  // We query the storage actor until it is initialized and use it to upload the assets
  const {actor: storageActor} = storageBucket;

  if (!storageActor) {
    return;
  }

  await uploadDecks({updateDecks, identity, dataActor: actor, storageBucket, syncWindow, log});

  await uploadSlides({updateSlides, identity, actor, storageBucket, syncWindow, log});

  await deleteSlides({deleteSlides: slidesToDelete, actor, log});

  await uploadDocs({updateDocs, actor, log});

  await uploadParagraphs({updateParagraphs, identity, actor, storageBucket, syncWindow, log});

  await deleteParagraphs({deleteParagraphs: paragraphsToDelete, actor, log});
};

const uploadDecks = async ({
  updateDecks,
  identity,
  dataActor,
  storageBucket,
  syncWindow,
  log
}: {
  updateDecks: SyncDataDeck[] | undefined;
  identity: Identity;
  dataActor: DataBucketActor;
  storageBucket: BucketActor<StorageBucketActor>;
  syncWindow: SyncWindow;
  log: LogWindow;
}) => {
  if (!updateDecks || updateDecks.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) =>
    uploadDeck({deck, dataActor, storageBucket, identity, syncWindow, log})
  );
  await Promise.all(promises);
};

const uploadDeck = async ({
  deck,
  dataActor,
  storageBucket,
  identity,
  syncWindow,
  log
}: {
  deck: Deck;
  dataActor: DataBucketActor;
  storageBucket: BucketActor<StorageBucketActor>;
  identity: Identity;
  syncWindow: SyncWindow;
  log: LogWindow;
}) => {
  if (!deck) {
    return;
  }

  // 1. We upload the asset to the IC (worker side), update DOM and IDB (window side for thread safe reason) and clean the asset from IDB
  const {src: imgSrc, storageFile}: SyncStorage = await uploadDeckBackgroundAssets({deck, identity, syncWindow, storageBucket, log});

  // 2. If we uploaded an asset, its URL has changed (no more local but available online)
  const updateDeck: Deck = updateDeckBackground({deck, imgSrc, storageFile});

  // 3. We can update the data in the IC
  await uploadDeckData({deck: updateDeck, actor: dataActor, log});
};

const uploadSlides = async ({
  updateSlides,
  identity,
  actor,
  syncWindow,
  storageBucket,
  log
}: {
  updateSlides: SyncDataSlide[] | undefined;
  identity: Identity;
  actor: DataBucketActor;
  syncWindow: SyncWindow;
  storageBucket: BucketActor<StorageBucketActor>;
  log: LogWindow;
}) => {
  if (!updateSlides || updateSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateSlides.map(({slide, deckId}: SyncDataSlide) =>
    uploadSlide({slide, deckId, actor, identity, storageBucket, syncWindow, log})
  );

  await Promise.all(promises);
};

const deleteSlides = async ({
  deleteSlides,
  actor,
  log
}: {
  deleteSlides: SyncDataSlide[] | undefined;
  actor: DataBucketActor;
  log: LogWindow;
}) => {
  if (!deleteSlides || deleteSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = deleteSlides
    .filter(({slideId}: SyncDataSlide) => slideId !== undefined)
    .map(({deckId, slideId}: SyncDataSlide) => deleteData({key: `/decks/${deckId}/slides/${slideId}`, actor, log}));

  await Promise.all(promises);
};

const uploadSlide = async ({
  slide,
  deckId,
  actor,
  identity,
  syncWindow,
  storageBucket,
  log
}: {
  slide: Slide;
  deckId: string;
  actor: DataBucketActor;
  identity: Identity;
  syncWindow: SyncWindow;
  storageBucket: BucketActor<StorageBucketActor>;
  log: LogWindow;
}) => {
  if (!slide) {
    return;
  }

  // 1. We upload the asset to the IC (worker side), update DOM and IDB (window side for thread safe reason) and clean the asset from IDB
  const {images, chart}: SyncStorageSlide = await uploadSlideAssets({slide, deckId, storageBucket, identity, syncWindow, log});

  // 2. If we uploaded assets, there URL have changed (no more local but available online)
  const updateChartSlide: Slide = updateSlideChart({slide, chart});
  const updateSlide: Slide = updateSlideImages({slide: updateChartSlide, images});

  // 3. We can update the data in the IC
  await uploadSlideData({deckId, slide: updateSlide, actor, log});
};

const uploadDocs = async ({updateDocs, actor, log}: {updateDocs: SyncDataDoc[] | undefined; actor: DataBucketActor; log: LogWindow}) => {
  if (!updateDocs || updateDocs.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateDocs.map(({doc}: SyncDataDoc) => uploadDoc({doc, actor, log}));
  await Promise.all(promises);
};

const uploadDoc = async ({doc, actor, log}: {doc: Doc; actor: DataBucketActor; log: LogWindow}) => {
  if (!doc) {
    return;
  }

  await uploadDocData({doc, actor, log});
};

const uploadParagraphs = async ({
  updateParagraphs,
  identity,
  actor,
  syncWindow,
  storageBucket,
  log
}: {
  updateParagraphs: SyncDataParagraph[] | undefined;
  identity: Identity;
  actor: DataBucketActor;
  syncWindow: SyncWindow;
  storageBucket: BucketActor<StorageBucketActor>;
  log: LogWindow;
}) => {
  if (!updateParagraphs || updateParagraphs.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateParagraphs.map(({paragraph, docId}: SyncDataParagraph) =>
    uploadParagraph({paragraph, docId, actor, identity, storageBucket, syncWindow, log})
  );

  await Promise.all(promises);
};

const uploadParagraph = async ({
  paragraph,
  docId,
  actor,
  identity,
  syncWindow,
  storageBucket,
  log
}: {
  paragraph: Paragraph;
  docId: string;
  actor: DataBucketActor;
  identity: Identity;
  syncWindow: SyncWindow;
  storageBucket: BucketActor<StorageBucketActor>;
  log: LogWindow;
}) => {
  if (!paragraph) {
    return;
  }

  // 1. We upload the asset to the IC (worker side), update DOM and IDB (window side for thread safe reason) and clean the asset from IDB
  const images: SyncStorage[] | undefined = await uploadParagraphImages({paragraph, docId, storageBucket, identity, syncWindow, log});

  // 2. If we uploaded assets, there URL have changed (no more local but available online)
  const updateParagraph: Paragraph = updateParagraphImages({paragraph: paragraph, images});

  // 3. We can update the data in the IC
  await uploadParagraphData({docId, paragraph: updateParagraph, actor, log});
};

const deleteParagraphs = async ({
  deleteParagraphs,
  actor,
  log
}: {
  deleteParagraphs: SyncDataParagraph[] | undefined;
  actor: DataBucketActor;
  log: LogWindow;
}) => {
  if (!deleteParagraphs || deleteParagraphs.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = deleteParagraphs
    .filter(({paragraphId}: SyncDataParagraph) => paragraphId !== undefined)
    .map(({docId, paragraphId}: SyncDataParagraph) => deleteData({key: `/docs/${docId}/paragraphs/${paragraphId}`, actor, log}));

  await Promise.all(promises);
};
