import * as admin from 'firebase-admin';
import * as functions from 'firebase-functions';

import {scheduleTask} from '../../utils/data/task-utils';
import {geToken} from '../utils/request-utils';

import {DeployData} from '../../model/data/deploy';

export interface ScheduledPublishTask {}

export function schedulePublish(request: functions.Request): Promise<ScheduledPublishTask> {
  return new Promise<ScheduledPublishTask>(async (resolve, reject) => {
    try {
      const token: string | undefined = await geToken(request);
      const deckId: string | undefined = request.body.deckId;
      const ownerId: string | undefined = request.body.ownerId;

      if (!deckId) {
        reject('No deck information provided.');
        return;
      }

      if (!token) {
        reject('No token provided.');
        return;
      }

      if (!ownerId) {
        reject('No owner ID provided.');
        return;
      }

      const publish: boolean = request.body.publish !== undefined && request.body.publish;
      const github: boolean = request.body.github !== undefined && request.body.github;

      if (!github && !publish) {
        reject('Nothing to publish');
        return;
      }

      // We tell the frontend to wait
      await scheduleDeploy(deckId, ownerId, publish, github);

      // We schedule internally / cloud the job so we keep secret the token

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

function scheduleDeploy(deckId: string, ownerId: string, publish: boolean, github: boolean): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deckId || deckId === undefined || !deckId) {
        resolve();
        return;
      }

      const documentReference: admin.firestore.DocumentReference = admin.firestore().doc(`/deploys/${deckId}/`);

      const updateData: DeployData = {
        owner_id: ownerId,
        updated_at: admin.firestore.Timestamp.now(),
      };

      if (publish) {
        updateData.api = {
          status: 'scheduled',
        };
      }

      if (github) {
        updateData.github = {
          status: 'scheduled',
        };
      }

      await documentReference.set(updateData, {merge: true});

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
