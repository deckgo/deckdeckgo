import {firebase} from '@firebase/app';
import '@firebase/firestore';

import {BehaviorSubject, Observable, ReplaySubject} from 'rxjs';
import {take} from 'rxjs/operators';

import {Deck, DeckData} from '../../../models/data/deck';

import {ErrorService} from '../../core/error/error.service';

export class FeedService {

    private static instance: FeedService;

    private decksSubject: ReplaySubject<Deck[]> = new ReplaySubject(1);
    private lastPageReached: BehaviorSubject<boolean> = new BehaviorSubject(false);

    private errorService: ErrorService;

    private nextQueryAfter: firebase.firestore.DocumentSnapshot;

    private queryLimit: number = 10;

    private decks: Deck[] = [];

    private constructor() {
        // Private constructor, singleton
        this.errorService = ErrorService.getInstance();
    }

    static getInstance() {
        if (!FeedService.instance) {
            FeedService.instance = new FeedService();
        }
        return FeedService.instance;
    }

    watchDecks(): Observable<Deck[]> {
        return this.decksSubject.asObservable();
    }

    watchLastPageReached(): Observable<boolean> {
        return this.lastPageReached.asObservable();
    }

    reset(): Promise<void> {
        return new Promise<void>((resolve) => {
            this.nextQueryAfter = null;
            this.decks = [];

            this.lastPageReached.next(false);
            this.decksSubject.next([]);

            resolve();
        });
    }

    refresh(): Promise<void> {
        return new Promise<void>(async  (resolve) => {
            this.nextQueryAfter = null;
            this.decks = [];

            await this.find();

            resolve();
        });
    }

    find(): Promise<void> {
        return new Promise<void>(async (resolve) => {
               this.watchLastPageReached().pipe(take(1)).subscribe(async (reached: boolean) => {
                  if (reached) {
                      resolve();
                      return;
                  }

                  await this.findNext();

                  resolve();
               });
        });
    }

    private findNext(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            try {
                const snapshot: firebase.firestore.QuerySnapshot = await this.query();

                if (!snapshot || !snapshot.docs || snapshot.docs.length <= 0) {
                    this.lastPageReached.next(true);

                    resolve();
                    return;
                }

                this.nextQueryAfter = snapshot.docs[snapshot.docs.length - 1];

                const decks: Deck[] = snapshot.docs.map((documentSnapshot: firebase.firestore.QueryDocumentSnapshot) => {
                    return {
                        id: documentSnapshot.id,
                        data: documentSnapshot.data() as DeckData
                    }
                });

                await this.addDecks(decks);

                resolve();
            } catch (err) {
                this.errorService.error('Something weird happened, we couldn\'t fetch the decks.');
                resolve();
            }
        });
    }

    private query(): Promise<firebase.firestore.QuerySnapshot> {
        const firestore: firebase.firestore.Firestore = firebase.firestore();

        if (this.nextQueryAfter) {
            return firestore.collection('decks')
                    .where('meta.feed', '==', true)
                    .orderBy('meta.published_at', 'desc')
                    .startAfter(this.nextQueryAfter)
                    .limit(this.queryLimit).get();
        } else {
            return firestore.collection('decks')
                    .where('meta.feed', '==', true)
                    .orderBy('meta.published_at', 'desc')
                    .limit(this.queryLimit).get();
        }
    }

    private addDecks(decks: Deck[]): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!decks || decks.length <= 0) {
                this.lastPageReached.next(true);

                resolve();
                return;
            }

            // It costs around half a second to randomize 10 cards with Chrome but makes the feed more dynamic
            const randomlySortedDecks: Deck[] = await this.shuffle(decks);
            this.decks = this.decks.concat(randomlySortedDecks);

            this.decksSubject.next(this.decks);

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
