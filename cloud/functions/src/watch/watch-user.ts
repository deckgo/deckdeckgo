import {Change, EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import * as admin from 'firebase-admin';

import {deleteDecksSlides} from './delete/delete-decks-slides';
import {deleteUserStorage} from './delete/delete-user-storage';
import {deletePlatform} from './delete/delete-platform';

import {createMailchimpMember, deleteMailchimpMember, updateMailchimpMember} from './mailchimp/mailchimp-member';

import {updateDeckMeta} from './update/update-deck-meta';

export async function applyWatchUserDelete(userRecord: admin.auth.UserRecord, _context: EventContext) {
  await deleteDecksSlides(userRecord);
  await deleteUserStorage(userRecord);
  await deleteMailchimpMember(userRecord);
  await deletePlatform(userRecord);
}

export async function applyWatchUserCreate(userRecord: admin.auth.UserRecord, _context: EventContext) {
  await createMailchimpMember(userRecord);
}

export async function applyWatchUserUpdate(change: Change<DocumentSnapshot>, context: EventContext) {
  await updateMailchimpMember(change);
  await updateDeckMeta(change, context);
}
