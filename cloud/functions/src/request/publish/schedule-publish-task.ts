import * as admin from 'firebase-admin';
import * as functions from 'firebase-functions';

import {scheduleTask} from '../../utils/data/task-utils';
import {getToken} from '../utils/request-utils';

import {DeckData, DeckDeployData} from '../../model/data/deck';

export interface ScheduledPublishTask {
  deckId: string;
  status: 'scheduled';
  publish: boolean;
  github: boolean;
}

export function schedulePublish(request: functions.Request): Promise<ScheduledPublishTask> {
  return new Promise<ScheduledPublishTask>(async (resolve, reject) => {
    try {
      const token: string | undefined = await getToken(request);
      const deckId: string | undefined = request.body.deckId;

      if (!deckId) {
        reject('No deck information provided.');
        return;
      }

      if (!token) {
        reject('No token provided.');
        return;
      }

      const publish: boolean = request.body.publish !== undefined && request.body.publish;
      const github: boolean = request.body.github !== undefined && request.body.github;

      if (!github && !publish) {
        reject('Nothing to publish');
        return;
      }

      // We tell the frontend to wait
      await updateDeckDeploy(deckId, publish, github);

      // We schedule internally / cloud the job so we keep secret the token
      await schedule(deckId, publish, github, token);

      resolve({
        deckId,
        status: 'scheduled',
        publish: publish,
        github: github,
      });
    } catch (err) {
      reject(err);
    }
  });
}

async function schedule(deckId: string, publish: boolean, github: boolean, token: string) {
  if (publish && github) {
    await scheduleTask({
      deckId,
      token,
      type: 'publish-all',
    });

    return;
  }

  if (publish) {
    await scheduleTask({
      deckId,
      token,
      type: 'publish-deck',
    });
  }

  if (github) {
    await scheduleTask({
      deckId,
      token,
      type: 'push-github',
    });
  }
}

function updateDeckDeploy(deckId: string, publish: boolean, github: boolean): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deckId || deckId === undefined || !deckId) {
        resolve();
        return;
      }

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/decks/${deckId}/`);

      const deployData: DeckDeployData = {
        status: 'scheduled',
        updated_at: admin.firestore.Timestamp.now(),
      };

      const updateData: Partial<DeckData> = publish
        ? {
            deploy: {
              api: {
                ...deployData,
              },
            },
          }
        : {
            deploy: {
              github: {
                ...deployData,
              },
            },
          };

      await documentReference.set(updateData, {merge: true});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
