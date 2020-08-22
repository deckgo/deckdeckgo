import * as admin from 'firebase-admin';

export async function deleteToken(userRecord: admin.auth.UserRecord) {
  if (!userRecord || !userRecord.uid || userRecord.uid === undefined || userRecord.uid === '') {
    return;
  }

  try {
    const userId: string = userRecord.uid;

    await deleteTokenDoc(userId);
  } catch (err) {
    console.error(err);
  }
}

function deleteTokenDoc(userId: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/tokens/`);
      const doc: admin.firestore.DocumentReference = collectionRef.doc(userId);

      await doc.delete();

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
