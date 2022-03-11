import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import {Deck} from '../../model/data/deck';
import {TaskData} from '../../model/data/task';
import {findDeck} from '../../utils/data/deck-utils';
import {failureTask, successfulTask} from '../../utils/data/task-utils';
import {publishToApi} from './api/publish-api';
import {publishToGitHub} from './github/publish-github';
import {generateDeckScreenshot} from './screenshot/generate-deck-screenshot';

export async function publish(snap: DocumentSnapshot, context: EventContext) {
  const taskId: string = context.params.taskId;

  if (!taskId || taskId === undefined || taskId === '') {
    return;
  }

  try {
    await publishJob(snap);

    await successfulTask(taskId);
  } catch (err) {
    console.error(err);

    await failureTask(taskId);
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

      if (task.type === 'publish-all') {
        const newPublish: boolean = deck.data.api_id === undefined || deck.data.api_id === null;

        // If we do both, currently we need the API first as we are getting the content from the published deck
        await publishToApi(deck, task.token as string);
        await generateDeckScreenshot(deck.id);

        // Even if we fixed the delay to publish to Cloudflare CDN (#195), sometimes if too quick, the presentation will not be correctly published
        // Therefore, to avoid such problem, we add a bit of delay in the process but only for the first publish
        setTimeout(
          async () => {
            await delayPublishToGitHub(deck.id);
            resolve();
          },
          newPublish ? 7000 : 0
        );
      } else if (task.type === 'publish-deck') {
        await publishToApi(deck, task.token as string);
        await generateDeckScreenshot(deck.id);
        resolve();
      } else if (task.type === 'push-github') {
        await publishToGitHub(deck.id, deck.data);
        resolve();
      }
    } catch (err) {
      reject(err);
    }
  });
}

async function delayPublishToGitHub(deckId: string) {
  // It has been changed by the publish to the API
  const refreshDeck: Deck = await findDeck(deckId);

  if (!refreshDeck || !refreshDeck.data) {
    throw new Error('Updated published deck cannot be found');
  }

  try {
    await publishToGitHub(refreshDeck.id, refreshDeck.data);
  } catch (err) {
    throw err;
  }
}
