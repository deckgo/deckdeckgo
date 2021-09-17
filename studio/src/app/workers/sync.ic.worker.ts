import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Deck, DeckData, Slide, SlideData, SyncData, SyncDataDeck, SyncDataSlide} from '@deckdeckgo/editor';

import {_SERVICE as ManagerActor} from '../canisters/manager/manager.did';
import {_SERVICE as DeckBucketActor} from '../canisters/deck/deck.did';

import {InternetIdentityAuth} from '../types/core/ic.identity';

import {createDeckBucketActor, createManagerActor, initDeckBucket} from '../utils/core/ic.deck.utils';
import {CanisterUtils} from '../utils/editor/canister.utils';
import {initIdentity} from '../utils/core/ic.identity.utils';

export const uploadWorker = async ({
  internetIdentity: {delegationChain, identityKey},
  syncData,
  host
}: {
  internetIdentity: InternetIdentityAuth;
  syncData: SyncData | undefined;
  host: string;
}) => {
  if (!syncData) {
    return;
  }

  if (!delegationChain || !identityKey) {
    return;
  }

  const identity: Identity = initIdentity({identityKey, delegationChain});

  const {updateDecks, updateSlides, deleteSlides: slidesToDelete} = syncData;

  const managerActor: ManagerActor = await createManagerActor({identity, host});

  await uploadDecks({updateDecks, identity, managerActor, host});

  await uploadSlides({updateSlides, identity, managerActor, host});

  await deleteSlides({deleteSlides: slidesToDelete, identity, managerActor, host});

  // TODO: handle delete decks here?
};

const uploadDecks = async ({
  updateDecks,
  identity,
  managerActor,
  host
}: {
  updateDecks: SyncDataDeck[] | undefined;
  identity: Identity;
  managerActor: ManagerActor;
  host: string;
}) => {
  if (!updateDecks || updateDecks.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) =>
    uploadDeck({deck, managerActor: managerActor, identity, host})
  );
  await Promise.all(promises);

  console.log('C synced');
};

const uploadDeck = async ({
  deck,
  managerActor,
  identity,
  host
}: {
  deck: Deck;
  managerActor: ManagerActor;
  identity: Identity;
  host: string;
}) => {
  if (!deck) {
    return;
  }

  console.log('Deck IC about to SET');
  const t0 = performance.now();

  const bucket: Principal = await initDeckBucket({managerActor, deckId: deck.id});

  const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket, host});

  await deckBucket.set({
    deckId: deck.id,
    data: await CanisterUtils.toArray<DeckData>(deck.data),
    created_at: CanisterUtils.toTimestamp((deck.data.created_at as Date) || new Date()),
    updated_at: CanisterUtils.toTimestamp((deck.data.updated_at as Date) || new Date())
  });

  const t1 = performance.now();
  console.log('Deck IC SET done', t1 - t0);

  const t2 = performance.now();

  // TODO: remove, just for test
  console.log('Deck IC Get:', await deckBucket.get(), performance.now() - t2);
};

const uploadSlides = async ({
  updateSlides,
  identity,
  managerActor,
  host
}: {
  updateSlides: SyncDataSlide[] | undefined;
  identity: Identity;
  managerActor: ManagerActor;
  host: string;
}) => {
  if (!updateSlides || updateSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateSlides.map(({slide, deckId}: SyncDataSlide) =>
    uploadSlide({slide, deckId, managerActor: managerActor, identity, host})
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
  host
}: {
  slide: Slide;
  deckId: string;
  managerActor: ManagerActor;
  identity: Identity;
  host: string;
}) => {
  if (!slide) {
    return;
  }

  console.log('Slide IC about to SET');
  const t0 = performance.now();

  const t4 = performance.now();
  const bucket: Principal = await initDeckBucket({managerActor, deckId});

  const t5 = performance.now();
  console.log('Bucket retrieved', t5 - t4);

  const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket, host});

  await deckBucket.setSlide({
    slideId: slide.id,
    data: await CanisterUtils.toArray<SlideData>(slide.data),
    created_at: CanisterUtils.toTimestamp((slide.data.created_at as Date) || new Date()),
    updated_at: CanisterUtils.toTimestamp((slide.data.updated_at as Date) || new Date())
  });

  const t1 = performance.now();
  console.log('Slide IC SET done', t1 - t0);

  const t2 = performance.now();

  // TODO: remove, just for test
  console.log('Slide IC Get:', await deckBucket.getSlide(slide.id), performance.now() - t2);
};

const deleteSlide = async ({
  slideId,
  deckId,
  managerActor,
  identity,
  host
}: {
  slideId: string;
  deckId: string;
  managerActor: ManagerActor;
  identity: Identity;
  host: string;
}) => {
  if (!slideId) {
    return;
  }

  console.log('Slide IC about to DEL');
  const t0 = performance.now();

  const bucket: Principal = await initDeckBucket({managerActor, deckId});

  const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket, host});

  await deckBucket.delSlide(slideId);

  const t1 = performance.now();
  console.log('Slide IC DEL done', t1 - t0);
};
