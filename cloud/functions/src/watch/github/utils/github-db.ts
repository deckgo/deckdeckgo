import * as admin from 'firebase-admin';

import {Token, TokenData} from '../../../model/token';

export function findToken(userId: string): Promise<Token> {
  return new Promise<Token>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/tokens/${userId}/`).get();

      if (!snapshot.exists) {
        reject('Token not found');
        return;
      }

      const tokenData: TokenData = snapshot.data() as TokenData;

      resolve({
        id: snapshot.id,
        ref: snapshot.ref,
        data: tokenData,
      });
    } catch (err) {
      reject(err);
    }
  });
}
