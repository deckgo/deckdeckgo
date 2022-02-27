import {Deck, DeckData, Doc, DocData, Paragraph, ParagraphData, Slide, SlideData} from '@deckdeckgo/editor';

import {_SERVICE as DataBucketActor} from '../canisters/data/data.did';

import {setData} from './data.utils';
import { LogWindow } from '../types/sync.window';

export const uploadDeckData = async ({deck, actor, log}: {deck: Deck; actor: DataBucketActor, log: LogWindow}) =>
  setData<Deck, DeckData>({key: `/decks/${deck.id}`, id: deck.id, data: deck.data, actor, log});

export const uploadSlideData = async ({deckId, slide, actor, log}: {deckId: string; slide: Slide; actor: DataBucketActor, log: LogWindow}) =>
  setData<Slide, SlideData>({key: `/decks/${deckId}/slides/${slide.id}`, id: slide.id, data: slide.data, actor, log});

export const uploadDocData = async ({doc, actor, log}: {doc: Doc; actor: DataBucketActor, log: LogWindow}) =>
  setData<Doc, DocData>({key: `/docs/${doc.id}`, id: doc.id, data: doc.data, actor, log});

export const uploadParagraphData = async ({docId, paragraph, actor, log}: {docId: string; paragraph: Paragraph; actor: DataBucketActor, log: LogWindow}) =>
  setData<Paragraph, ParagraphData>({key: `/docs/${docId}/paragraphs/${paragraph.id}`, id: paragraph.id, data: paragraph.data, actor, log});
