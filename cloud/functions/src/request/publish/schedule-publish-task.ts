import * as functions from 'firebase-functions';

import {scheduleTask} from '../utils/task-utils';
import {geToken} from '../utils/request-utils';

export interface ScheduledPublishTask {}

export function schedulePublish(request: functions.Request): Promise<ScheduledPublishTask> {
  return new Promise<ScheduledPublishTask>(async (resolve, reject) => {
    try {
      const token: string | undefined = await geToken(request);
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

      if (publish) {
        await scheduleTask({
          deckId,
          token,
          type: 'publish-deck',
        });
      }

      const github: boolean = request.body.github !== undefined && request.body.github;

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
