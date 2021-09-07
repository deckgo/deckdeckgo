import firebase from 'firebase/app';
import 'firebase/firestore';

import {Deck, DeckData} from '../../models/data/deck';
import {Slide} from '../../models/data/slide';

import {importEditorData} from '../../utils/editor/import.utils';
import {FirestoreUtils} from '../../utils/editor/firestore.utils';

import {DeckFirebaseProvider} from '../../providers/data/deck/deck.firebase.provider';
import {getSlideService, SlideService} from '../../providers/data/slide/slide.service';

export interface DeckDashboardCloneResult {
  from: Deck;
  to: Deck;
}

export class DeckDashboardService {
  private static instance: DeckDashboardService;

  private deckFirebaseProvider: DeckFirebaseProvider;
  private slideService: SlideService;

  private constructor() {
    this.deckFirebaseProvider = DeckFirebaseProvider.getInstance();
    this.slideService = getSlideService();
  }

  static getInstance() {
    if (!DeckDashboardService.instance) {
      DeckDashboardService.instance = new DeckDashboardService();
    }
    return DeckDashboardService.instance;
  }

  clone(deck: Deck): Promise<DeckDashboardCloneResult> {
    return new Promise<DeckDashboardCloneResult>(async (resolve, reject) => {
      try {
        const clone: Deck = await this.cloneDeck(deck);

        deck.data.clone = {
          deck_id_to: clone.id
        };

        const updatedDeck: Deck = await this.deckFirebaseProvider.update(deck);

        resolve({
          from: updatedDeck,
          to: clone
        });
      } catch (err) {
        reject(err);
      }
    });
  }

  private cloneDeck(deck: Deck): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        let clone: DeckData = {...deck.data};

        clone.clone = {
          deck_id_from: deck.id
        };

        delete clone['slides'];
        delete clone['api_id'];
        delete clone['meta'];

        const createdDeck: Deck = await this.deckFirebaseProvider.create(clone);

        resolve(createdDeck);
      } catch (err) {
        reject(err);
      }
    });
  }

  snapshot(deck: Deck, updateFunction: Function): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!deck || !deck.id) {
        resolve();
        return;
      }

      const firestore: firebase.firestore.Firestore = firebase.firestore();
      const unsubscribe = firestore
        .collection('decks')
        .doc(deck.id)
        .onSnapshot((deckSnapshot: firebase.firestore.DocumentSnapshot<DeckData>) => {
          updateFunction(
            {
              id: deckSnapshot.id,
              data: deckSnapshot.data()
            },
            unsubscribe
          );
        });

      resolve();
    });
  }

  importData(deck: Deck): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const promises: Promise<Slide>[] | undefined = deck.data.slides?.map((slideId: string) => this.slideService.get(deck.id, slideId));
        const slides: Slide[] = await Promise.all(promises || []);

        const deckToImport: Deck = {...deck};

        if (deckToImport.data.background && FirestoreUtils.shouldAttributeBeCleaned(deckToImport.data.background)) {
          deckToImport.data.background = null;
        }

        if (deckToImport.data.header && FirestoreUtils.shouldAttributeBeCleaned(deckToImport.data.header)) {
          deckToImport.data.header = null;
        }

        if (deckToImport.data.footer && FirestoreUtils.shouldAttributeBeCleaned(deckToImport.data.footer)) {
          deckToImport.data.footer = null;
        }

        await importEditorData({
          id: deck.id,
          deck,
          slides
        });

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }
}
