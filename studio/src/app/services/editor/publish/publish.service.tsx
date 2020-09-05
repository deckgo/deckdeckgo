import * as firebase from 'firebase/app';
import 'firebase/firestore';
import 'firebase/auth';

import deckStore from '../../../stores/deck.store';
import userStore from '../../../stores/user.store';
import errorStore from '../../../stores/error.store';
import authStore from '../../../stores/auth.store';

import {Deck, DeckData, DeckMetaAuthor} from '../../../models/data/deck';

import {UserSocial} from '../../../models/data/user';

import {DeckService} from '../../data/deck/deck.service';

import {EnvironmentConfigService} from '../../core/environment/environment-config.service';
import {EnvironmentFirebaseConfig} from '../../core/environment/environment-config';

export class PublishService {
  private static instance: PublishService;

  private deckService: DeckService;

  private constructor() {
    this.deckService = DeckService.getInstance();
  }

  static getInstance() {
    if (!PublishService.instance) {
      PublishService.instance = new PublishService();
    }
    return PublishService.instance;
  }

  publish(description: string, tags: string[], github: boolean): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (!deckStore.state.deck || !deckStore.state.deck.id || !deckStore.state.deck.data) {
          reject('No deck found');
          return;
        }

        await this.updateDeckMeta(description, tags, github);

        await this.publishDeck(deckStore.state.deck);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private publishDeck(deck: Deck): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const config: EnvironmentFirebaseConfig = EnvironmentConfigService.getInstance().get('firebase');

        const token: string = await firebase.auth().currentUser.getIdToken();

        const rawResponse: Response = await fetch(`${config.functionsUrl}/publish`, {
          method: 'POST',
          headers: {
            Accept: 'application/json',
            'Content-Type': 'application/json',
            Authorization: `Bearer ${token}`,
          },
          body: JSON.stringify({
            deckId: deck.id,
            ownerId: deck.data.owner_id,
            publish: true,
            github: deck.data.github ? deck.data.github.publish : false,
          }),
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
  }

  private updateDeckMeta(description: string, tags: string[], github: boolean): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (!userStore.state.user || !userStore.state.user.data) {
          reject('No user');
          return;
        }

        const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();

        const deck: Deck = {...deckStore.state.deck};

        if (!deck.data.meta) {
          deck.data.meta = {
            title: deck.data.name,
            updated_at: now,
          };
        } else {
          deck.data.meta.title = deck.data.name;
          deck.data.meta.updated_at = now;
        }

        if (description && description !== undefined && description !== '') {
          deck.data.meta.description = description;
        } else {
          deck.data.meta.description = firebase.firestore.FieldValue.delete();
        }

        if (!tags || tags.length <= 0) {
          deck.data.meta.tags = firebase.firestore.FieldValue.delete();
        } else {
          deck.data.meta.tags = tags;
        }

        if (userStore.state.user && userStore.state.user.data && userStore.state.user.data.name) {
          if (!deck.data.meta.author) {
            deck.data.meta.author = {
              name: userStore.state.user.data.name,
            };
          } else {
            (deck.data.meta.author as DeckMetaAuthor).name = userStore.state.user.data.name;
          }

          if (userStore.state.user.data.photo_url) {
            (deck.data.meta.author as DeckMetaAuthor).photo_url = userStore.state.user.data.photo_url;
          }

          if (userStore.state.user.data.social) {
            (deck.data.meta.author as DeckMetaAuthor).social = Object.keys(userStore.state.user.data.social).reduce((acc: UserSocial, key: string) => {
              // @ts-ignore
              acc[key] =
                userStore.state.user.data.social[key] !== null && userStore.state.user.data.social[key] !== undefined
                  ? userStore.state.user.data.social[key]
                  : firebase.firestore.FieldValue.delete();
              return acc;
            }, {} as UserSocial);
          } else {
            (deck.data.meta.author as DeckMetaAuthor).social = firebase.firestore.FieldValue.delete();
          }
        } else if (deck.data.meta.author) {
          deck.data.meta.author = firebase.firestore.FieldValue.delete();
        }

        // Update GitHub info (push or not) for GitHub users so next time user publish, the choice is kept
        if (authStore.state.gitHub) {
          if (deck.data.github) {
            deck.data.github.publish = github;
          } else {
            deck.data.github = {
              publish: github,
            };
          }
        }

        const updatedDeck: Deck = await this.deckService.update(deckStore.state.deck);
        deckStore.state.deck = {...updatedDeck};

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  snapshot(): Promise<() => void | undefined> {
    return new Promise<() => void | undefined>((resolve) => {
      const deck: Deck = deckStore.state.deck;

      if (!deck || !deck.id) {
        resolve(undefined);
        return;
      }

      const firestore: firebase.firestore.Firestore = firebase.firestore();
      const unsubscribe = firestore
        .collection(`decks`)
        .doc(deck.id)
        .onSnapshot(
          (deploySnapshot: firebase.firestore.DocumentSnapshot<DeckData>) => {
            deckStore.state.deck = {
              id: deploySnapshot.id,
              data: deploySnapshot.data(),
            };
          },
          (_err) => {
            errorStore.state.error = 'Cannont retrieve the deck information.';
          }
        );

      resolve(unsubscribe);
    });
  }
}
