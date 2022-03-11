import * as functions from 'firebase-functions';
import fetch, {Response} from 'node-fetch';
import {ApiDeck} from '../../model/api/api.deck';
import {ApiPresentation} from '../../model/api/api.presentation';
import {Deck} from '../../model/data/deck';

export function publishDeckApi(deck: Deck, apiDeck: ApiDeck, token: string): Promise<ApiPresentation> {
  return new Promise<ApiPresentation>(async (resolve, reject) => {
    try {
      const apiDeckPublish: ApiPresentation = await createOrUpdatePublish(deck, apiDeck, token);

      resolve(apiDeckPublish);
    } catch (err) {
      reject(err);
    }
  });
}

function createOrUpdatePublish(deck: Deck, apiDeck: ApiDeck, token: string): Promise<ApiPresentation> {
  if (deck.data.api_id) {
    return put(deck.data.api_id, apiDeck, token);
  } else {
    return post(apiDeck, token);
  }
}

function post(deck: ApiDeck, token: string): Promise<ApiPresentation> {
  return query(deck, '/presentations', 'POST', token);
}

function put(apiDeckId: string, deck: ApiDeck, token: string): Promise<ApiPresentation> {
  return query(deck, `/presentations/${apiDeckId}`, 'PUT', token);
}

function query(deck: ApiDeck, context: string, method: string, token: string): Promise<ApiPresentation> {
  return new Promise<ApiPresentation>(async (resolve, reject) => {
    try {
      const apiSkip: string = functions.config().deckdeckgo.api.skip;

      if (apiSkip === 'true') {
        resolve(mockApiPresentationData());
        return;
      }

      const apiUrl: string = functions.config().deckdeckgo.api.url;

      const rawResponse: Response = await fetch(apiUrl + context, {
        method: method,
        headers: {
          Accept: 'application/json',
          'Content-Type': 'application/json',
          Authorization: `Bearer ${token}`,
        },
        body: JSON.stringify(deck),
      });

      if (!rawResponse || !rawResponse.ok) {
        reject('Something went wrong while creating or updating the deck');
        return;
      }

      const publishedPresentation: ApiPresentation = (await rawResponse.json()) as ApiPresentation;

      resolve(publishedPresentation);
    } catch (err) {
      reject(err);
    }
  });
}

function mockApiPresentationData(): ApiPresentation {
  const presentationUrl: string = functions.config().deckdeckgo.presentation.url;

  return {
    id: Math.random().toString(36).substring(2) + Date.now().toString(36),
    url: `${presentationUrl}/daviddalbusco/introducing-deckdeckgo/`,
  };
}
