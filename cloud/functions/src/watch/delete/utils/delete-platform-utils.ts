import * as admin from 'firebase-admin';

export function deleteDeployForId(deckId: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deckId || !deckId || deckId === undefined) {
        resolve();
        return;
      }

      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/deploys/`);
      const doc: admin.firestore.DocumentReference = collectionRef.doc(deckId);

      await doc.delete();

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
