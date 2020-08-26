import * as admin from 'firebase-admin';

import {Platform, PlatformData} from '../../../model/platform';

export function findPlatform(userId: string): Promise<Platform> {
  return new Promise<Platform>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/platforms/${userId}/`).get();

      if (!snapshot.exists) {
        reject('Platform not found');
        return;
      }

      const data: PlatformData = snapshot.data() as PlatformData;

      resolve({
        id: snapshot.id,
        ref: snapshot.ref,
        data,
      });
    } catch (err) {
      reject(err);
    }
  });
}
