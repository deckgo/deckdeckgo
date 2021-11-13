import {Deck, DeckData, Doc, DocData, Paragraph, ParagraphData, Slide, SlideData} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor, Data} from '../canisters/data/data.did';

import {toArray, toTimestamp} from './did.utils';

export const uploadDeckData = async ({deck, actor}: {deck: Deck; actor: DataBucketActor}) => {
  await uploadData({
    key: `/decks/${deck.id}`,
    data: {
      id: deck.id,
      data: await toArray<DeckData>(deck.data),
      created_at: toTimestamp((deck.data.created_at as Date) || new Date()),
      updated_at: toTimestamp((deck.data.updated_at as Date) || new Date())
    },
    actor
  });
};

export const uploadSlideData = async ({deckId, slide, actor}: {deckId: string; slide: Slide; actor: DataBucketActor}) => {
  await uploadData({
    key: `/decks/${deckId}/slides/${slide.id}`,
    data: {
      id: slide.id,
      data: await toArray<SlideData>(slide.data),
      created_at: toTimestamp((slide.data.created_at as Date) || new Date()),
      updated_at: toTimestamp((slide.data.updated_at as Date) || new Date())
    },
    actor
  });
};

export const uploadDocData = async ({doc, actor}: {doc: Doc; actor: DataBucketActor}) => {
  await uploadData({
    key: `/docs/${doc.id}`,
    data: {
      id: doc.id,
      data: await toArray<DocData>(doc.data),
      created_at: toTimestamp((doc.data.created_at as Date) || new Date()),
      updated_at: toTimestamp((doc.data.updated_at as Date) || new Date())
    },
    actor
  });
};

export const uploadParagraphData = async ({docId, paragraph, actor}: {docId: string; paragraph: Paragraph; actor: DataBucketActor}) => {
  await uploadData({
    key: `/docs/${docId}/paragraphs/${paragraph.id}`,
    data: {
      id: paragraph.id,
      data: await toArray<ParagraphData>(paragraph.data),
      created_at: toTimestamp((paragraph.data.created_at as Date) || new Date()),
      updated_at: toTimestamp((paragraph.data.updated_at as Date) || new Date())
    },
    actor
  });
};

const uploadData = async ({data, key, actor}: {data: Data; key: string; actor: DataBucketActor}) => {
  console.log(`Data IC (${key}) about to SET`);
  const t0 = performance.now();

  await actor.set(key, data);

  const t1 = performance.now();
  console.log(`Data IC SET (${key}) done:`, t1 - t0);

  const t2 = performance.now();

  // TODO: remove, just for test
  console.log(`Data IC GET (${key}):`, await actor.get(key), performance.now() - t2);
};

export const deleteData = async ({key, actor}: {key: string; actor: DataBucketActor}) => {
  console.log('Data IC about to DEL');
  const t0 = performance.now();

  await actor.del(key);

  const t1 = performance.now();
  console.log('Data IC DEL done', t1 - t0);
};
