import * as admin from 'firebase-admin';

import {Token, TokenData} from '../../../../model/data/token';
import {DeployGitHubRepo, Deploy, DeployData, DeployGitHub} from '../../../../model/data/deploy';

export function findToken(userId: string): Promise<Token> {
  return new Promise<Token>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/tokens/${userId}/`).get();

      if (!snapshot.exists) {
        reject('Platform not found');
        return;
      }

      const data: TokenData = snapshot.data() as TokenData;

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

export function findDeploy(deckId: string): Promise<Deploy | undefined> {
  return new Promise<Deploy | undefined>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/deploys/${deckId}`).get();

      if (!snapshot.exists) {
        resolve(undefined);
        return;
      }

      const data: DeployData = snapshot.data() as DeployData;

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

export function updateGitHubDeploy(deckId: string, deckData: DeployData, repo: DeployGitHubRepo | undefined): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deckId || deckId === undefined || deckId === '') {
        resolve();
        return;
      }

      if (!repo) {
        resolve();
        return;
      }

      const data: DeployData = {...deckData};
      (data.github as DeployGitHub).repo = {...repo};

      data.updated_at = admin.firestore.Timestamp.now();

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/deploys/${deckId}`);

      await documentReference.set(data, {merge: true});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
