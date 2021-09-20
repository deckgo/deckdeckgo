import firebase from 'firebase/app';
import '@firebase/auth';
import 'firebase/firestore';

import {Deck, DeckMetaAuthor, Publish, UserSocial} from '@deckdeckgo/editor';

import {updateDeck} from '../data/deck.firebase';

export const publish: Publish = async ({deck: deckSource, config}: {deck: Deck; config: Record<string, string>}): Promise<Deck> => {
  // 1. Update deck meta information
  const deck: Deck = await updateDeckMeta(deckSource);

  // 2. Trigger the function that effectively publish
  await publishDeck({deck, config});

  return deck;
};

const updateDeckMeta = async (deckSource: Deck): Promise<Deck> => {
  const deck: Deck = cleanDeckMeta(deckSource);

  return updateDeck(deck);
};

const cleanDeckMeta = (deckSource: Deck): Deck => {
  const deck: Deck = {...deckSource};

  const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();

  deck.data.meta.updated_at = now;

  if (deck.data.meta.description === null) {
    deck.data.meta.description = firebase.firestore.FieldValue.delete() as unknown as string;
  }

  if (deck.data.meta.tags === null || deck.data.meta.tags.length <= 0) {
    deck.data.meta.tags = firebase.firestore.FieldValue.delete() as unknown as string[];
  }

  if (deck.data.meta.author === null) {
    deck.data.meta.author = firebase.firestore.FieldValue.delete() as unknown as DeckMetaAuthor;
  } else if (deck.data.meta.author.social === null) {
    (deck.data.meta.author as DeckMetaAuthor).social = firebase.firestore.FieldValue.delete() as unknown as UserSocial;
  } else {
    (deck.data.meta.author as DeckMetaAuthor).social = Object.keys((deck.data.meta.author as DeckMetaAuthor).social).reduce(
      (acc: UserSocial, key: string) => {
        acc[key] =
          (deck.data.meta.author as DeckMetaAuthor).social[key] !== null
            ? (deck.data.meta.author as DeckMetaAuthor).social[key]
            : firebase.firestore.FieldValue.delete();
        return acc;
      },
      {} as UserSocial
    );
  }

  return deck;
};

const publishDeck = ({deck, config}: {deck: Deck; config: Record<string, string | boolean>}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const token: string = await firebase.auth().currentUser.getIdToken();

      const rawResponse: Response = await fetch(`${config.functionsUrl}/publish`, {
        method: 'POST',
        headers: {
          Accept: 'application/json',
          'Content-Type': 'application/json',
          Authorization: `Bearer ${token}`
        },
        body: JSON.stringify({
          deckId: deck.id,
          ownerId: deck.data.owner_id,
          publish: true,
          github: deck.data.github ? deck.data.github.publish : false
        })
      });

      if (!rawResponse || !rawResponse.ok) {
        reject('Something went wrong while publishing the deck');
        return;
      }

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};
