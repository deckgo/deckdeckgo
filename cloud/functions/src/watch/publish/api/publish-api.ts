import * as admin from 'firebase-admin';

import {Deck, DeckData, DeckMeta} from '../../../model/data/deck';
import {ApiDeck} from '../../../model/api/api.deck';
import {ApiPresentation} from '../../../model/api/api.presentation';

import {convertDeck} from '../../../request/utils/convert-deck-utils';
import {publishDeckApi} from '../../../request/utils/api-utils';
import {updateDeck} from '../../../utils/data/deck-utils';
import {failureDeploy, successfulDeploy} from '../../../utils/data/deck-deploy-utils';

import {Resources} from '../../../utils/resources/resources';

export function publishToApi(deck: Deck, token: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deck || !deck.data) {
        reject('No deck data.');
        return;
      }

      if (!deck.data.meta) {
        reject('No deck meta data.');
        return;
      }

      const apiDeck: ApiDeck = await convertDeck(deck);

      if (!apiDeck) {
        reject('Cannot convert deck.');
        return;
      }

      const apiDeckPublish: ApiPresentation = await publishDeckApi(deck, apiDeck, token);

      if (!apiDeckPublish || !apiDeckPublish.id || !apiDeckPublish.url) {
        reject(`Publish for deck ${deck.id} failed.`);
        return;
      }

      await updateDeckPublished(deck, apiDeckPublish);

      await successfulDeploy(deck.id, 'api');

      resolve();
    } catch (err) {
      await failureDeploy(deck.id, 'api');

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
