import * as functions from 'firebase-functions';

import {Deck} from '../../model/deck';
import {ApiDeck} from '../../model/api/api.deck';

import {findDeck} from '../../utils/deck-utils';
import {convertDeck} from '../utils/convert-deck-utils';

export function publishDeck(request: functions.Request): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const deckId: string = request.body.deckId;

      if (!deckId) {
        reject('No deck information provided');
        return;
      }

      const deck: Deck = await findDeck(deckId);

      if (!deck) {
        reject('No matching deck');
        return;
      }

      const apiDeck: ApiDeck = await convertDeck(deck);

      console.log('API DECK', apiDeck);
    } catch (err) {
      reject(err);
    }
  });
}
