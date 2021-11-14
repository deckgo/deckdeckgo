import {Deck, DeckData, Doc, DocData, Paragraph, ParagraphData, Slide, SlideData} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';

import {setData} from './data.utils';

export const uploadDeckData = async ({deck, actor}: {deck: Deck; actor: DataBucketActor}) =>
  setData<Deck, DeckData>({key: `/decks/${deck.id}`, id: deck.id, data: deck.data, actor});

export const uploadSlideData = async ({deckId, slide, actor}: {deckId: string; slide: Slide; actor: DataBucketActor}) =>
  setData<Slide, SlideData>({key: `/decks/${deckId}/slides/${slide.id}`, id: slide.id, data: slide.data, actor});

export const uploadDocData = async ({doc, actor}: {doc: Doc; actor: DataBucketActor}) =>
  setData<Doc, DocData>({key: `/docs/${doc.id}`, id: doc.id, data: doc.data, actor});

export const uploadParagraphData = async ({docId, paragraph, actor}: {docId: string; paragraph: Paragraph; actor: DataBucketActor}) =>
  setData<Paragraph, ParagraphData>({key: `/docs/${docId}/paragraphs/${paragraph.id}`, id: paragraph.id, data: paragraph.data, actor});

export const deleteData = async ({key, actor}: {key: string; actor: DataBucketActor}) => deleteData({key, actor});
