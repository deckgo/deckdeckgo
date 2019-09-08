import {EventContext} from 'firebase-functions';

import * as admin from 'firebase-admin';

import {deleteDecksSlides} from './delete/delete-decks-slides';
import {deleteUserStorage} from './delete/delete-user-storage';
import {deleteUser} from './delete/delete-user';


export async function applyWatchUserDelete(userRecord: admin.auth.UserRecord, _context: EventContext) {
    await deleteDecksSlides(userRecord);
    await deleteUserStorage(userRecord);
    await deleteUser(userRecord);
}
