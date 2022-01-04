import firebase from 'firebase/app';
import '@firebase/auth';
import 'firebase/firestore';

import {User as FirebaseUser} from '@firebase/auth-types';

import {Deck, Author, DeckPublish, UserSocial, PublishUrl, DocPublish, Doc} from '@deckdeckgo/editor';

import {updateDeck} from '../data/deck.firebase';

export const deckPublish: DeckPublish = async ({deck: deckSource, config}: {deck: Deck; config: Record<string, string>}): Promise<Deck> => {
  // 1. Update deck meta information
  const deck: Deck = await updateDeckMeta(deckSource);

  // 2. Trigger the function that effectively publish
  await publishDeck({deck, config});

  return deck;
};

export const docPublish: DocPublish = async (_params: {doc: Doc}): Promise<Doc> => {
  throw new Error('Publish operation not supported.');
};

export const publishUrl: PublishUrl = async () => 'https://beta.deckdeckgo.io';

const updateDeckMeta = async (deckSource: Deck): Promise<Deck> => {
  const deck: Deck = cleanDeckMeta(deckSource);

  const ownerDeck: Deck = updateOwner(deck);

  return updateDeck(ownerDeck);
};

/**
 * If deck was created before user was logged in, the owner_id might have not been set in the offline data
 */
const updateOwner = (deckSource: Deck): Deck => {
  if (deckSource.data.owner_id !== undefined) {
    return deckSource;
  }

  const deck: Deck = {...deckSource};

  const firebaseUser: FirebaseUser = firebase.auth().currentUser;
  deck.data.owner_id = firebaseUser?.uid;

  return deck;
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
    deck.data.meta.author = firebase.firestore.FieldValue.delete() as unknown as Author;
  } else if (deck.data.meta.author.social === null) {
    (deck.data.meta.author as Author).social = firebase.firestore.FieldValue.delete() as unknown as UserSocial;
  } else {
    (deck.data.meta.author as Author).social = Object.keys((deck.data.meta.author as Author).social).reduce(
      (acc: UserSocial, key: string) => {
        acc[key] =
          (deck.data.meta.author as Author).social[key] !== null
            ? (deck.data.meta.author as Author).social[key]
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
      const token: string | undefined = await firebase.auth().currentUser?.getIdToken();

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
