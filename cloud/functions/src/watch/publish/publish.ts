import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {Deck} from '../../model/data/deck';
import {TaskData} from '../../model/data/task';

import {findDeck} from '../../utils/data/deck-utils';

import {failure, successful} from '../../utils/data/task-utils';
import {publishToApi} from './api/publish-api';
import {publishToGitHub} from './github/publish-github';

export async function publish(snap: DocumentSnapshot, context: EventContext) {
  const taskId: string = context.params.taskId;

  if (!taskId || taskId === undefined || taskId === '') {
    return;
  }

  try {
    await publishJob(snap);

    await successful(taskId);
  } catch (err) {
    console.error(err);

    await failure(taskId);
  }
}

function publishJob(snap: DocumentSnapshot): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    const task: TaskData = snap.data() as TaskData;

    if (!task || task === undefined) {
      reject('No task data.');
      return;
    }

    if (!task.deckId || !task.token) {
      reject('No task token.');
      return;
    }

    if (task.status !== 'scheduled') {
      reject('Task not scheduled.');
      return;
    }

    try {
      const deck: Deck = await findDeck(task.deckId);

      if (!deck || !deck.data) {
        reject('Task found no deck data.');
        return;
      }

      if (task.type === 'publish-deck') {
        await publishToApi(deck, task.token as string);
      } else if (task.type === 'push-github') {
        await publishToGitHub(deck.id, deck.data);
      }

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
