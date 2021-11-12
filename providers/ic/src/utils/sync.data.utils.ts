import {Deck, DeckData, Slide, SlideData} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';

import {toArray, toTimestamp} from './did.utils';

export const uploadDeckData = async ({deck, actor}: {deck: Deck; actor: DataBucketActor}) => {
  console.log('Deck IC about to SET');
  const t0 = performance.now();

  const key: string = `/decks/${deck.id}`;

  await actor.set(key, {
    id: deck.id,
    data: await toArray<DeckData>(deck.data),
    created_at: toTimestamp((deck.data.created_at as Date) || new Date()),
    updated_at: toTimestamp((deck.data.updated_at as Date) || new Date())
  });

  const t1 = performance.now();
  console.log('Deck IC SET done', t1 - t0);

  const t2 = performance.now();

  // TODO: remove, just for test
  console.log('Deck IC Get:', await actor.get(key), performance.now() - t2);
};

export const uploadSlideData = async ({slide, deckId, actor}: {slide: Slide; deckId: string; actor: DataBucketActor}) => {
  console.log('Slide IC about to SET');
  const t0 = performance.now();

  const t4 = performance.now();

  const t5 = performance.now();
  console.log('Bucket retrieved', t5 - t4);

  const key: string = `/decks/${deckId}/slides/${slide.id}`;

  await actor.set(key, {
    id: slide.id,
    data: await toArray<SlideData>(slide.data),
    created_at: toTimestamp((slide.data.created_at as Date) || new Date()),
    updated_at: toTimestamp((slide.data.updated_at as Date) || new Date())
  });

  const t1 = performance.now();
  console.log('Slide IC SET done', t1 - t0);

  const t2 = performance.now();

  // TODO: remove, just for test
  console.log('Slide IC Get:', await actor.get(key), performance.now() - t2);
};

export const deleteSlide = async ({slideId, deckId, actor}: {slideId: string; deckId: string; actor: DataBucketActor}) => {
  if (!slideId) {
    return;
  }

  console.log('Slide IC about to DEL');
  const t0 = performance.now();

  const key: string = `/decks/${deckId}/slides/${slideId}`;

  await actor.del(key);

  const t1 = performance.now();
  console.log('Slide IC DEL done', t1 - t0);
};
