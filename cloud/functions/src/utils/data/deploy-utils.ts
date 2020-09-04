import * as admin from 'firebase-admin';

import {DeployApi, DeployData, DeployGitHub} from '../../model/data/deploy';

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

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/deploys/${deckId}/`);

      const updateData: Partial<DeployData> = {
        updated_at: admin.firestore.Timestamp.now(),
      };

      (updateData[type] as DeployGitHub | DeployApi).status = status;

      await admin.firestore().runTransaction((transaction) => {
        return transaction.get(documentReference).then((sfDoc) => {
          if (!sfDoc.exists) {
            throw 'Document does not exist!';
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
