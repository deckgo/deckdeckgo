import {Identity} from '@dfinity/agent';
import {Principal} from '@dfinity/principal';

import {Deck, DeckData, Slide, SlideData} from '@deckdeckgo/editor';

import {_SERVICE as ManagerActor} from '../canisters/manager/manager.did';
import {_SERVICE as DeckBucketActor} from '../canisters/deck/deck.did';

import {createDeckBucketActor, initDeckBucket} from './manager.utils';
import {toArray, toTimestamp} from './did.utils';

export const uploadDeckData = async ({
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
  console.log('Deck IC about to SET');
  const t0 = performance.now();

  const bucket: Principal = await initDeckBucket({managerActor, deckId: deck.id});

  const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket, host});

  await deckBucket.set({
    deckId: deck.id,
    data: await toArray<DeckData>(deck.data),
    created_at: toTimestamp((deck.data.created_at as Date) || new Date()),
    updated_at: toTimestamp((deck.data.updated_at as Date) || new Date())
  });

  const t1 = performance.now();
  console.log('Deck IC SET done', t1 - t0);

  const t2 = performance.now();

  // TODO: remove, just for test
  console.log('Deck IC Get:', await deckBucket.get(), performance.now() - t2);
};

export const uploadSlideData = async ({
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
  console.log('Slide IC about to SET');
  const t0 = performance.now();

  const t4 = performance.now();
  const bucket: Principal = await initDeckBucket({managerActor, deckId});

  const t5 = performance.now();
  console.log('Bucket retrieved', t5 - t4);

  const deckBucket: DeckBucketActor = await createDeckBucketActor({identity, bucket, host});

  await deckBucket.setSlide({
    slideId: slide.id,
    data: await toArray<SlideData>(slide.data),
    created_at: toTimestamp((slide.data.created_at as Date) || new Date()),
    updated_at: toTimestamp((slide.data.updated_at as Date) || new Date())
  });

  const t1 = performance.now();
  console.log('Slide IC SET done', t1 - t0);

  const t2 = performance.now();

  // TODO: remove, just for test
  console.log('Slide IC Get:', await deckBucket.getSlide(slide.id), performance.now() - t2);
};

export const deleteSlide = async ({
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
