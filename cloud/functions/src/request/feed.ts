import * as functions from 'firebase-functions';

import {Deck, DeckMeta, DeckMetaAuthor} from '../model/data/deck';

import {findPublishedDecks} from '../utils/data/deck-utils';
import {getDateObj} from '../utils/utils';

interface FeedData {
  title: string;

  pathname: string;

  description?: string;
  tags?: string[];
  author?: DeckMetaAuthor;

  published_at: Date;
}

interface Feed {
  id: string;
  data: FeedData;
}

export async function feedDecks(request: functions.Request, response: functions.Response<any>) {
  const isValidBearer: boolean = await validBearer(request);

  if (!isValidBearer) {
    response.status(400).json({
      error: 'Not Authorized',
    });
    return;
  }

  try {
    const decks: Deck[] = await findPublishedDecks();

    const feed: Feed[] = decks
      .map((deck: Deck) => {
        const meta: DeckMeta = deck.data.meta as DeckMeta;

        return {
          id: deck.id,
          data: {
            title: meta.title,
            pathname: meta.pathname,
            published_at: getDateObj(meta.published_at) as Date,
            ...(meta.description && {description: meta.description as string}),
            ...(meta.tags && {tags: meta.tags as string[]}),
            ...(meta.author && {author: meta.author as DeckMetaAuthor}),
          },
        };
      })
      .sort((a: Feed, b: Feed) => {
        return new Date(b.data.published_at).getTime() - new Date(a.data.published_at).getTime();
      });

    response.json(feed);
  } catch (err) {
    response.status(500).json({
      error: err,
    });
  }
}

async function validBearer(request: functions.Request): Promise<boolean> {
  const key: string = functions.config().feed.token;

  const authorization = request.get('Authorization');
  const split = authorization ? authorization.split('Bearer ') : [];
  const bearerKey = split && split.length >= 2 ? split[1] : undefined;

  return key === bearerKey;
}
