import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';
import {DelegationChain, DelegationIdentity, Ed25519KeyIdentity} from '@dfinity/identity';

import {_SERVICE as DecksActor, _SERVICE as DecskActor} from '../canisters/decks/decks.did';
import {_SERVICE as DeckBucketActor} from '../canisters/deck/deck.did';

import {Deck, DeckData} from '../models/data/deck';
import {Slide, SlideData} from '../models/data/slide';

import {SyncData, SyncDataDeck, SyncDataSlide} from '../types/editor/sync';

import {createDeckBucketActor, createDecksActor} from '../utils/core/ic.deck.utils';
import {CanisterUtils} from '../utils/editor/canister.utils';

export const uploadWorker = async ({
  delegationChain,
  identityKey,
  syncData,
  host
}: {
  identityKey: string | null;
  delegationChain: string | null;
  syncData: SyncData | undefined;
  host: string;
}) => {
  if (!syncData) {
    return;
  }

  if (!delegationChain || !identityKey) {
    return;
  }

  const chain: DelegationChain = DelegationChain.fromJSON(delegationChain);
  const key: Ed25519KeyIdentity = Ed25519KeyIdentity.fromJSON(identityKey);

  const identity: Identity = DelegationIdentity.fromDelegation(key, chain);

  const {updateDecks, updateSlides, deleteSlides: slidesToDelete} = syncData;

  const decksActor: DecskActor = await createDecksActor({identity, host});

  await uploadDecks({updateDecks, identity, decksActor, host});

  await uploadSlides({updateSlides, identity, decksActor, host});

  await deleteSlides({deleteSlides: slidesToDelete, identity, decksActor, host});

  // TODO: handle delete decks here?
};

const uploadDecks = async ({
  updateDecks,
  identity,
  decksActor,
  host
}: {
  updateDecks: SyncDataDeck[] | undefined;
  identity: Identity;
  decksActor: DecskActor;
  host: string;
}) => {
  if (!updateDecks || updateDecks.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateDecks.map(({deck}: SyncDataDeck) => uploadDeck({deck, decksActor, identity, host}));
  await Promise.all(promises);

  console.log('C synced');
};

const uploadDeck = async ({deck, decksActor, identity, host}: {deck: Deck; decksActor: DecksActor; identity: Identity; host: string}) => {
  if (!deck) {
    return;
  }

  console.log('Deck IC about to SET');
  const t0 = performance.now();

  const bucket: Principal = await decksActor.init(deck.id);

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
  decksActor,
  host
}: {
  updateSlides: SyncDataSlide[] | undefined;
  identity: Identity;
  decksActor: DecskActor;
  host: string;
}) => {
  if (!updateSlides || updateSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = updateSlides.map(({slide, deckId}: SyncDataSlide) => uploadSlide({slide, deckId, decksActor, identity, host}));
  await Promise.all(promises);
};

const deleteSlides = async ({
  deleteSlides,
  identity,
  decksActor,
  host
}: {
  deleteSlides: SyncDataSlide[] | undefined;
  identity: Identity;
  decksActor: DecskActor;
  host: string;
}) => {
  if (!deleteSlides || deleteSlides.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = deleteSlides.map(({deckId, slideId}: SyncDataSlide) => deleteSlide({slideId, deckId, identity, decksActor, host}));
  await Promise.all(promises);
};

const uploadSlide = async ({
  slide,
  deckId,
  decksActor,
  identity,
  host
}: {
  slide: Slide;
  deckId: string;
  decksActor: DecksActor;
  identity: Identity;
  host: string;
}) => {
  if (!slide) {
    return;
  }

  console.log('Slide IC about to SET');
  const t0 = performance.now();

  const bucket: Principal = await decksActor.init(deckId);

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
  decksActor,
  identity,
  host
}: {
  slideId: string;
  deckId: string;
  decksActor: DecksActor;
  identity: Identity;
  host: string;
}) => {
  if (!slideId) {
    return;
  }

  console.log('Slide IC about to DEL');
  const t0 = performance.now();

  const bucket: Principal = await decksActor.init(deckId);

  const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket, host});

  await deckBucket.delSlide(slideId);

  const t1 = performance.now();
  console.log('Slide IC DEL done', t1 - t0);
};
