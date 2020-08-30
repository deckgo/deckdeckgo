import * as admin from 'firebase-admin';

import {Deploy} from '../../model/deploy';

import {deleteDeployForId} from './utils/delete-platform-utils';

export async function deleteDeploy(userRecord: admin.auth.UserRecord) {
  if (!userRecord || !userRecord.uid || userRecord.uid === undefined || userRecord.uid === '') {
    return;
  }

  try {
    const userId: string = userRecord.uid;

    const deploys: Deploy[] | null = await findDeploys(userId);

    if (!deploys || deploys.length <= 0) {
      return;
    }

    const promises: Promise<void>[] = deploys.map((deploy: Deploy) => deleteDeployForId(deploy.id));
    await Promise.all(promises);
  } catch (err) {
    console.error(err);
  }
}

function findDeploys(userId: string): Promise<Deploy[] | null> {
  return new Promise<Deploy[] | null>(async (resolve, reject) => {
    try {
      if (!userId || userId === undefined || userId === '') {
        resolve(null);
        return;
      }

      const collectionRef: admin.firestore.CollectionReference = admin.firestore().collection(`/deploys/`);

      const snapShot: admin.firestore.QuerySnapshot = await collectionRef.where('owner_id', '==', userId).get();

      if (snapShot && snapShot.docs && snapShot.docs.length > 0) {
        const decks: Deploy[] = snapShot.docs.map((doc) => {
          const id = doc.id;
          const ref = doc.ref;

          return {
            id: id,
            ref: ref,
            data: doc.data(),
          } as Deploy;
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
