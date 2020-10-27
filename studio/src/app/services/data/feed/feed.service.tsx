import firebase from 'firebase/app';
import 'firebase/firestore';

import feedStore from '../../../stores/feed.store';
import errorStore from '../../../stores/error.store';

import {Deck, DeckData} from '../../../models/data/deck';

export class FeedService {
  private static instance: FeedService;

  private nextQueryAfter: firebase.firestore.DocumentSnapshot;

  private queryLimit: number = 20;

  static getInstance() {
    if (!FeedService.instance) {
      FeedService.instance = new FeedService();
    }
    return FeedService.instance;
  }

  refresh(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.nextQueryAfter = null;

      feedStore.reset();

      await this.find();

      resolve();
    });
  }

  async find() {
    if (feedStore.state.lastPageReached) {
      return;
    }

    await this.findNext();
  }

  private findNext(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        const snapshot: firebase.firestore.QuerySnapshot = await this.query();

        if (!snapshot || !snapshot.docs || snapshot.docs.length <= 0) {
          feedStore.state.lastPageReached = true;

          resolve();
          return;
        }

        this.nextQueryAfter = snapshot.docs[snapshot.docs.length - 1];

        const decks: Deck[] = snapshot.docs.map((documentSnapshot: firebase.firestore.QueryDocumentSnapshot) => {
          return {
            id: documentSnapshot.id,
            data: documentSnapshot.data() as DeckData,
          };
        });

        await this.addDecks(decks);

        resolve();
      } catch (err) {
        errorStore.state.error = "Something weird happened, we couldn't fetch the decks.";
        resolve();
      }
    });
  }

  private query(): Promise<firebase.firestore.QuerySnapshot> {
    const firestore: firebase.firestore.Firestore = firebase.firestore();

    if (this.nextQueryAfter) {
      return firestore
        .collection('decks')
        .where('meta.feed', '==', true)
        .orderBy('meta.published_at', 'desc')
        .startAfter(this.nextQueryAfter)
        .limit(this.queryLimit)
        .get();
    } else {
      return firestore.collection('decks').where('meta.feed', '==', true).orderBy('meta.published_at', 'desc').limit(this.queryLimit).get();
    }
  }

  private addDecks(decks: Deck[]): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!decks || decks.length <= 0) {
        feedStore.state.lastPageReached = true;

        resolve();
        return;
      }

      // It costs around half a second to randomize 10 cards with Chrome but makes the feed more dynamic
      const randomlySortedDecks: Deck[] = await this.shuffle(decks);
      const updatedDecks: Deck[] = feedStore.state.decks ? feedStore.state.decks.concat(randomlySortedDecks) : randomlySortedDecks;

      feedStore.state.decks = [...updatedDecks];

      resolve();
    });
  }

  // https://stackoverflow.com/a/12646864/5404186
  private shuffle(decks: Deck[]): Promise<Deck[]> {
    return new Promise<Deck[]>((resolve) => {
      for (let i = decks.length - 1; i > 0; i--) {
        const j: number = Math.floor(Math.random() * (i + 1));
        [decks[i], decks[j]] = [decks[j], decks[i]];
      }

      resolve(decks);
    });
  }
}
