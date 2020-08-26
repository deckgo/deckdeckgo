import * as admin from 'firebase-admin';

import {PlatformDeck} from '../../model/platform-deck';

import {deletePlatformDeckForIds} from './utils/delete-platform-utils';

export async function deletePlatform(userRecord: admin.auth.UserRecord) {
  if (!userRecord || !userRecord.uid || userRecord.uid === undefined || userRecord.uid === '') {
    return;
  }

  try {
    const userId: string = userRecord.uid;

    const platformDecks: PlatformDeck[] | null = await findPlatformDecks(userId);

    if (platformDecks && platformDecks.length > 0) {
      const promises: Promise<void>[] = platformDecks.map((platformDeck: PlatformDeck) => deletePlatformDeckForIds(userId, platformDeck.id));
      await Promise.all(promises);
    }

    await deletePlatformDoc(userId);
  } catch (err) {
    console.error(err);
  }
}

function deletePlatformDoc(userId: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/platforms/`);
      const doc: admin.firestore.DocumentReference = collectionRef.doc(userId);

      await doc.delete();

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function findPlatformDecks(userId: string): Promise<PlatformDeck[] | null> {
  return new Promise<PlatformDeck[] | null>(async (resolve, reject) => {
    try {
      if (!userId || userId === undefined || userId === '') {
        resolve(null);
        return;
      }

      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/platforms/${userId}/decks/`);

      const snapShot: admin.firestore.QuerySnapshot = await collectionRef.get();

      if (snapShot && snapShot.docs && snapShot.docs.length > 0) {
        const decks: PlatformDeck[] = snapShot.docs.map((doc) => {
          const id = doc.id;
          const ref = doc.ref;

          return {
            id: id,
            ref: ref,
            data: doc.data(),
          } as PlatformDeck;
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
