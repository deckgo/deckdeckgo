import * as functions from 'firebase-functions';

import {applyDeckUpdate} from './watch/watch-deck-update';

functions.firestore.document('decks/{deckId}').onUpdate(applyDeckUpdate);
