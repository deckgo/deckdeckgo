import * as admin from 'firebase-admin';

export async function deleteUserStorage(userRecord: admin.auth.UserRecord) {
  if (!userRecord || !userRecord.uid || userRecord.uid === undefined || userRecord.uid === '') {
    return;
  }

  try {
    const userId: string = userRecord.uid;

    await deleteStorage(userId);
  } catch (err) {
    console.error(err);
  }
}

function deleteStorage(userId: string | null): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!userId || userId === undefined || userId === '') {
        resolve();
        return;
      }

      const bucket = admin.storage().bucket();
      await bucket.deleteFiles({prefix: `${userId}/`});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
