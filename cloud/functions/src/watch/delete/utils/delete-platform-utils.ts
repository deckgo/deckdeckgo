import * as admin from 'firebase-admin';

export function deletePlatformDeckForIds(userId: string, deckId: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!userId || !userId || userId === undefined) {
        resolve();
        return;
      }

      if (!deckId || !deckId || deckId === undefined) {
        resolve();
        return;
      }

      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/platforms/${userId}/decks/`);
      const doc: admin.firestore.DocumentReference = collectionRef.doc(deckId);

      await doc.delete();

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
