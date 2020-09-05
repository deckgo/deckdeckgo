import * as admin from 'firebase-admin';

import {DeckData} from '../../model/data/deck';

export function successfulDeploy(deckId: string, type: 'github' | 'api'): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      await updateStatus(deckId, type, 'successful');

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

export function failureDeploy(deckId: string, type: 'github' | 'api'): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      await updateStatus(deckId, type, 'failure');

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function updateStatus(deckId: string, type: 'github' | 'api', status: 'scheduled' | 'failure' | 'successful'): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deckId || deckId === undefined || !deckId) {
        resolve();
        return;
      }

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/decks/${deckId}/`);

      const updateData: Partial<DeckData> = {
        updated_at: admin.firestore.Timestamp.now(),
      };

      if (type === 'github') {
        updateData.deploy = {
          github: {
            status,
            updated_at: admin.firestore.Timestamp.now(),
          },
        };
      } else if (type === 'api') {
        updateData.deploy = {
          api: {
            status,
            updated_at: admin.firestore.Timestamp.now(),
          },
        };
      }

      await admin.firestore().runTransaction((transaction) => {
        return transaction.get(documentReference).then((sfDoc) => {
          if (!sfDoc.exists) {
            throw new Error('Document does not exist!');
          }

          transaction.set(documentReference, updateData, {merge: true});
        });
      });

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
