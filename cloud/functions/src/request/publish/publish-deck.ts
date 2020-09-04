import * as functions from 'firebase-functions';

import {Deck} from '../../model/deck';
import {ApiDeck} from '../../model/api/api.deck';

import {findDeck} from '../../utils/deck-utils';
import {convertDeck} from '../utils/convert-deck-utils';

export async function publishDeck(request: functions.Request) {
  const deckId: string = request.body.deckId;

  if (!deckId) {
    throw new Error('No deck information provided');
  }

  const deck: Deck = await findDeck(deckId);

  if (!deck) {
    throw new Error('No matching deck');
  }

  const apiDeck: ApiDeck = await convertDeck(deck);

  console.log('API DECK', apiDeck);
}
