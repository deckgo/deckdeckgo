import * as functions from 'firebase-functions';

import {Deck} from '../../model/deck';
import {ApiDeck} from '../../model/api/api.deck';

import {findDeck} from '../../utils/deck-utils';
import {convertDeck} from '../utils/convert-deck-utils';
import {verifyToken} from '../utils/request-utils';

export async function publishDeck(request: functions.Request, response: functions.Response<any>) {
  const validToken: boolean = await verifyToken(request);

  if (!validToken) {
    response.status(400).json({
      error: 'Not Authorized',
    });
    return;
  }

  try {
    const deckId: string = request.body.deckId;

    if (!deckId) {
      response.status(500).json({
        error: 'No deck information provided',
      });
      return;
    }

    const deck: Deck = await findDeck(deckId);

    if (!deck) {
      response.status(500).json({
        error: 'No matching deck',
      });
      return;
    }

    const apiDeck: ApiDeck = await convertDeck(deck);

    console.log('API DECK', apiDeck);

    response.json({
      result: 'success',
    });
  } catch (err) {
    response.status(500).json({
      error: err,
    });
  }
}
