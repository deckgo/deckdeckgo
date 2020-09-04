import {Deck, DeckData, DeckMeta} from '../../model/data/deck';
import {ApiDeck} from '../../model/api/api.deck';
import {ApiPresentation} from '../../model/api/api.presentation';

import {findDeck, updateDeck} from '../../utils/data/deck-utils';
import {convertDeck} from '../utils/convert-deck-utils';
import {publishDeckApi} from '../utils/api-utils';
import {Resources} from '../../utils/resources/resources';
import * as admin from 'firebase-admin';

export function publishDeck(deckId: string | undefined, token: string | undefined): Promise<ApiPresentation> {
  return new Promise<ApiPresentation>(async (resolve, reject) => {
    try {
      if (!deckId) {
        reject('No deck information provided.');
        return;
      }

      if (!token) {
        reject('No token provided.');
        return;
      }

      const deck: Deck = await findDeck(deckId);

      if (!deck || !deck.data) {
        reject('No matching deck.');
        return;
      }

      if (!deck.data.meta) {
        reject('Deck has no meta.');
        return;
      }

      const apiDeck: ApiDeck = await convertDeck(deck);

      if (!apiDeck) {
        reject('No converted deck.');
        return;
      }

      const apiDeckPublish: ApiPresentation = await publishDeckApi(deck, apiDeck, token);

      if (!apiDeckPublish || !apiDeckPublish.id || !apiDeckPublish.url) {
        reject(`Publish for deck ${deck.id} failed.`);
        return;
      }

      await updateDeckPublished(deck, apiDeckPublish);

      resolve(apiDeckPublish);
    } catch (err) {
      reject(err);
    }
  });
}

function updateDeckPublished(deck: Deck, apiDeckPublish: ApiPresentation): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const url: URL = new URL(apiDeckPublish.url);

      const now: admin.firestore.Timestamp = admin.firestore.Timestamp.now();

      const deckData: Partial<DeckData> = {
        meta: {
          feed: deck.data.slides && deck.data.slides.length > Resources.Constants.FEED.MIN_SLIDES,
          pathname: url.pathname,
          updated_at: now,
        } as DeckMeta,
        api_id: apiDeckPublish.id,
      };

      const newApiId: boolean = deck.data.api_id !== apiDeckPublish.id;

      // First time published or updated
      if (newApiId) {
        (deckData.meta as DeckMeta).published = true;
        (deckData.meta as DeckMeta).published_at = now;
      }

      await updateDeck(deck.id, deckData);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
