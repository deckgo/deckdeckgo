import firebase from 'firebase/app';
import 'firebase/firestore';

import {Deck} from '../../models/data/deck';

export class DeckImportService {
  private static instance: DeckImportService;

  private constructor() {}

  static getInstance() {
    if (!DeckImportService.instance) {
      DeckImportService.instance = new DeckImportService();
    }
    return DeckImportService.instance;
  }

  snapshot(userId: string, updateFunction: Function): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!userId) {
        resolve();
        return;
      }

      // We listen to the next deck added to the collection for the user and for its first modification.
      // That's what happens in the cloud when extracting the data. The update is applied to the deck with the list of slides when these are created.

      const firestore: firebase.firestore.Firestore = firebase.firestore();
      const unsubscribe = firestore
        .collection('decks')
        .where('owner_id', '==', userId)
        .where('updated_at', '>', firebase.firestore.Timestamp.now())
        .orderBy('updated_at', 'desc')
        .onSnapshot((deckSnapshot: firebase.firestore.QuerySnapshot<Deck>) => {
          deckSnapshot.docChanges().forEach((change: firebase.firestore.DocumentChange<Deck>) => {
            if (change.type === 'modified') {
              updateFunction(
                {
                  id: change.doc.id,
                  data: change.doc.data()
                },
                unsubscribe
              );
            }
          });
        });

      resolve();
    });
  }
}
