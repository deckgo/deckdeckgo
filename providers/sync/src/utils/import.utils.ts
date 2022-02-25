import {Deck, Doc, Paragraph, Slide} from '@deckdeckgo/editor';
import {setEditDeckId, setEditDocId} from '@deckdeckgo/offline';
import {set, setMany} from 'idb-keyval';
import { syncUpdateDeck, syncUpdateDoc, syncUpdateParagraph, syncUpdateSlide } from './sync.utils';
import { ImportAsset, ImportData } from '../types/import.types';

export const importEditorData = async ({id, deck, slides, doc, paragraphs}: ImportData) => {
  if (!deck && !doc) {
    throw new Error('No deck or doc to import');
  }

  if (deck) {
    await setMany(slides?.map((slide: Slide) => [`/decks/${id}/slides/${slide.id}`, slide]));
    await set(`/decks/${id}`, deck);

    await setEditDeckId(id);

    return;
  }

  await setMany(paragraphs?.map((paragraph: Paragraph) => [`/docs/${id}/paragraphs/${paragraph.id}`, paragraph]));
  await set(`/docs/${id}`, doc);

  await setEditDocId(id);
};

export const importEditorAssets = (assets: ImportAsset[]): Promise<void> => {
  return setMany(assets.map(({path, blob}: ImportAsset) => [path, blob]));
};

export const importEditorSync = async ({deck, slides, doc, paragraphs}: ImportData) => {
  if (!deck && !doc) {
    throw new Error('No deck or doc to import');
  }

  if (deck) {
    await syncUpdateDeck(deck.id);

    for (let {id: slideId} of slides) {
      await syncUpdateSlide({deckId: deck.id, slideId});
    }

    return;
  }

  await syncUpdateDoc(doc.id);

  for (let {id: paragraphId} of paragraphs) {
    await syncUpdateParagraph({docId: doc.id, paragraphId});
  }
};

export const cleanDeck = ({deck, cleanMeta}: {deck: Deck; cleanMeta: boolean}): Deck => {
  const clone: Deck = {...deck};

  if (cleanMeta) {
    delete clone.data.meta;
  }

  delete clone.data.api_id;
  delete clone.data.deploy;
  delete clone.data.github;

  return clone;
};

export const cleanDoc = ({doc, cleanMeta}: {doc: Doc; cleanMeta: boolean}): Doc => {
  const clone: Doc = {...doc};

  if (cleanMeta) {
    delete clone.data.meta;
  }

  delete clone.data.deploy;

  return clone;
};
