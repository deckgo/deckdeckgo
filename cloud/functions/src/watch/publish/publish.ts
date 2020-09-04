import * as admin from 'firebase-admin';
import {EventContext} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {Deck, DeckData, DeckMeta} from '../../model/data/deck';
import {ApiDeck} from '../../model/api/api.deck';
import {ApiPresentation} from '../../model/api/api.presentation';
import {TaskData} from '../../model/data/task';

import {findDeck, updateDeck} from '../../utils/data/deck-utils';
import {convertDeck} from '../../request/utils/convert-deck-utils';
import {publishDeckApi} from '../../request/utils/api-utils';

import {Resources} from '../../utils/resources/resources';
import {failure, successful} from '../../utils/data/task-utils';

export async function publish(snap: DocumentSnapshot, context: EventContext) {
  const taskId: string = context.params.taskId;

  if (!taskId || taskId === undefined || taskId === '') {
    return;
  }

  const task: TaskData = snap.data() as TaskData;

  if (!task || task === undefined) {
    return;
  }

  if (!task.deckId || !task.token) {
    return;
  }

  if (task.status !== 'scheduled') {
    return;
  }

  try {
    const deck: Deck = await findDeck(task.deckId);

    if (!deck || !deck.data) {
      return;
    }

    if (!deck.data.meta) {
      return;
    }

    const apiDeck: ApiDeck = await convertDeck(deck);

    if (!apiDeck) {
      console.error('Cannot convert deck.');
      return;
    }

    const apiDeckPublish: ApiPresentation = await publishDeckApi(deck, apiDeck, task.token as string);

    if (!apiDeckPublish || !apiDeckPublish.id || !apiDeckPublish.url) {
      console.error(`Publish for deck ${deck.id} failed.`);
      return;
    }

    await updateDeckPublished(deck, apiDeckPublish);

    await successful(taskId);
  } catch (err) {
    console.error(err);

    await failure(taskId);
  }
}

function updateDeckPublished(deck: Deck, apiDeckPublish: ApiPresentation): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const url: URL = new URL(apiDeckPublish.url);

      const now: admin.firestore.Timestamp = admin.firestore.Timestamp.now();

      const deckData: Partial<DeckData> = {
        meta: {
          feed: deck.data.slides && deck.data.slides.length > Resources.Constants.FEED.MIN_SLIDES,
          pathname: url.pathname,
          updated_at: now,
        } as DeckMeta,
        api_id: apiDeckPublish.id,
      };

      const newApiId: boolean = deck.data.api_id !== apiDeckPublish.id;

      // First time published or updated
      if (newApiId) {
        (deckData.meta as DeckMeta).published = true;
        (deckData.meta as DeckMeta).published_at = now;
      }

      await updateDeck(deck.id, deckData);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}
