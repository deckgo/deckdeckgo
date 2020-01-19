import {Change, EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import * as admin from 'firebase-admin';

import {deleteDecksSlides} from './delete/delete-decks-slides';
import {deleteUserStorage} from './delete/delete-user-storage';

import {createMailchimpMember, deleteMailchimpMember, updateMailchimpMember} from './mailchimp/mailchimp-member';

export async function applyWatchUserDelete(userRecord: admin.auth.UserRecord, _context: EventContext) {
    await deleteDecksSlides(userRecord);
    await deleteUserStorage(userRecord);
    await deleteMailchimpMember(userRecord);
}

export async function applyWatchUserCreate(userRecord: admin.auth.UserRecord, _context: EventContext) {
    await createMailchimpMember(userRecord);
}

export async function applyWatchUserUpdate(change: Change<DocumentSnapshot>, _context: EventContext) {
    await updateMailchimpMember(change);
}
