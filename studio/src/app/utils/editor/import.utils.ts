import {Deck, Doc, Paragraph, Slide} from '@deckdeckgo/editor';
import {setEditDeckId, setEditDocId} from '@deckdeckgo/offline';
import {syncUpdateDeck, syncUpdateDoc, syncUpdateParagraph, syncUpdateSlide} from '@deckdeckgo/sync';
import {set, setMany} from 'idb-keyval';

export interface ImportData {
  id: string;
  deck?: Deck;
  slides?: Slide[];
  doc?: Doc;
  paragraphs?: Paragraph[];
}

export interface ImportAsset {
  path: string;
  blob: Blob;
}

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
