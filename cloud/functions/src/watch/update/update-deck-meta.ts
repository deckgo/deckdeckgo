import {Change, EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import * as admin from 'firebase-admin';

import {UserData, UserSocial} from '../../model/data/user';
import {Deck, DeckData, DeckMetaAuthor} from '../../model/data/deck';

export async function updateDeckMeta(change: Change<DocumentSnapshot>, context: EventContext) {
  const userId: string = context.params.userId;

  if (!userId || userId === undefined || userId === '') {
    return;
  }

  const newValue: UserData = change.after.data() as UserData;

  const previousValue: UserData = change.before.data() as UserData;

  if (!updateAuthor(previousValue, newValue)) {
    return;
  }

  try {
    const decks: Deck[] | null = await findPublishedDecks(userId);

    if (!decks || decks.length <= 0) {
      return;
    }

    const promises: Promise<void>[] = decks.map((deck: Deck) => updateDeckMetaAuthor(deck, newValue));

    await Promise.all(promises);
  } catch (err) {
    console.error(err);
  }
}

function updateAuthor(previousValue: UserData, newValue: UserData): boolean {
  if (!newValue || !previousValue) {
    return false;
  }

  if (!newValue.social && !previousValue.social) {
    return false;
  }

  if (JSON.stringify(previousValue.social) !== JSON.stringify(newValue.social)) {
    return true;
  }

  if (previousValue.photo_url !== newValue.photo_url) {
    return true;
  }

  return previousValue.name !== newValue.name;
}

function findPublishedDecks(userId: string): Promise<Deck[] | null> {
  return new Promise<Deck[] | null>(async (resolve, reject) => {
    try {
      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection('/decks/');

      const snapShot: admin.firestore.QuerySnapshot = await collectionRef.where('owner_id', '==', userId).where('meta.published', '==', true).get();

      if (snapShot && snapShot.docs && snapShot.docs.length > 0) {
        const decks: Deck[] = snapShot.docs.map((doc) => {
          const data: Object = doc.data() as DeckData;
          const id = doc.id;
          const ref = doc.ref;

          return {
            id: id,
            ref: ref,
            data: data,
          } as Deck;
        });

        resolve(decks);
      } else {
        resolve(null);
      }
    } catch (err) {
      reject(err);
    }
  });
}

function updateDeckMetaAuthor(deck: Deck, userData: UserData): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deck || !deck.id || deck.id === undefined || deck.id === '') {
        resolve();
        return;
      }

      if (!userData) {
        resolve();
        return;
      }

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/decks/${deck.id}/`);

      const updateData: Partial<DeckData> = {
        meta: {
          author: {
            name: userData.name,
          },
        },
        updated_at: admin.firestore.Timestamp.now(),
      } as Partial<DeckData>;

      if (!updateData.meta) {
        return;
      }

      if (userData.photo_url) {
        (updateData.meta.author as DeckMetaAuthor).photo_url = userData.photo_url;
      } else {
        (updateData.meta.author as DeckMetaAuthor).photo_url = admin.firestore.FieldValue.delete();
      }

      if (userData.social && userData.social !== undefined) {
        (updateData.meta.author as DeckMetaAuthor).social = Object.keys(userData.social).reduce((acc: UserSocial, key: string) => {
          // @ts-ignore
          acc[key] = userData.social[key] !== null && userData.social[key] !== undefined ? userData.social[key] : admin.firestore.FieldValue.delete();
          return acc;
        }, {} as UserSocial);
      } else {
        (updateData.meta.author as DeckMetaAuthor).social = admin.firestore.FieldValue.delete();
      }

      await documentReference.set(updateData, {merge: true});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
