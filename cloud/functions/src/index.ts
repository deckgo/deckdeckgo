import * as functions from 'firebase-functions';

import {applyDeckUpdate} from './watch/watch-deck-update';

export const watchDeckUpdate = functions.firestore.document('decks/{deckId}').onUpdate(applyDeckUpdate);
