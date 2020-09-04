import {Deck} from '../../model/deck';
import {ApiDeck} from '../../model/api/api.deck';
import {ApiPresentation} from '../../model/api/api.presentation';

import {findDeck} from '../../utils/deck-utils';
import {convertDeck} from '../utils/convert-deck-utils';
import {publishDeckApi} from '../utils/api-utils';

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

      if (!deck) {
        reject('No matching deck.');
        return;
      }

      const apiDeck: ApiDeck = await convertDeck(deck);

      if (!apiDeck) {
        reject('No converted deck.');
        return;
      }

      const apiDeckPublish: ApiPresentation = await publishDeckApi(deck, apiDeck, token);

      resolve(apiDeckPublish);
    } catch (err) {
      reject(err);
    }
  });
}
