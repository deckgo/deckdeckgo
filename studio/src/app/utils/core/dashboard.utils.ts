import {v4 as uuid} from 'uuid';

import {Deck, Slide, DeckData} from '@deckdeckgo/editor';

import {importEditorData} from '../editor/import.utils';

import {getSlide} from '../../providers/data/slide/slide.provider';
import {syncUpdateDeck, syncUpdateSlide} from '../editor/sync.utils';

export const clone = async (deck: Deck) => {
  const cloneDeck: Deck = cloneDeckData(deck);

  const promises: Promise<Slide>[] | undefined = deck.data.slides?.map((slideId: string) => getSlide(deck.id, slideId));
  const slides: Slide[] = await Promise.all(promises || []);

  const cloneSlides: Slide[] = slides.map((slide: Slide) => ({
    ...slide,
    id: uuid()
  }));

  cloneDeck.data.slides = cloneSlides.map(({id}: Slide) => id);

  await importEditorData({
    id: cloneDeck.id,
    deck: cloneDeck,
    slides: cloneSlides
  });

  // Add new deck and slides to list of data to sync
  await syncUpdateDeck(cloneDeck.id);

  for (let {id: slideId} of cloneSlides) {
    await syncUpdateSlide({deckId: cloneDeck.id, slideId});
  }
};

const cloneDeckData = (deck: Deck): Deck => {
  let clone: DeckData = {...deck.data};

  delete clone['slides'];
  delete clone['api_id'];
  delete clone['meta'];
  delete clone['deploy'];
  delete clone['github'];

  const now: Date = new Date();

  return {
    id: uuid(),
    data: {
      ...clone,
      updated_at: now,
      created_at: now
    }
  };
};

export const loadAndImport = (deck: Deck): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const promises: Promise<Slide>[] | undefined = deck.data.slides?.map((slideId: string) => getSlide(deck.id, slideId));
      const slides: Slide[] = await Promise.all(promises || []);

      await importEditorData({
        id: deck.id,
        deck,
        slides
      });

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};
