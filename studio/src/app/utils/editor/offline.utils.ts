import firebase from 'firebase/app';

import {SlideAttributes} from '../../models/data/slide';
import {DeckAttributes} from '../../models/data/deck';

import {FirestoreUtils} from './firestore.utils';

export class OfflineUtils {
  static async cleanAttributes(attributes: SlideAttributes | DeckAttributes): Promise<SlideAttributes | DeckAttributes> {
    if (!attributes || attributes === undefined) {
      return null;
    }

    const keys: string[] = Object.keys(attributes);

    if (!keys || keys.length <= 0) {
      return null;
    }

    return FirestoreUtils.filterDelete(attributes, true);
  }

  static prepareAttributes(attributes: SlideAttributes | DeckAttributes): Promise<SlideAttributes | DeckAttributes> {
    return new Promise<SlideAttributes | DeckAttributes>((resolve) => {
      if (!attributes) {
        // @ts-ignore
        resolve(firebase.firestore.FieldValue.delete());
        return;
      }

      const keys: string[] = Object.keys(attributes);

      if (!keys || keys.length <= 0) {
        // @ts-ignore
        resolve(firebase.firestore.FieldValue.delete());
        return;
      }

      keys.forEach((key: string) => {
        const attr = attributes[key];

        // Replace null values with Firestore "to delete fields"
        if (attr === null) {
          // @ts-ignore
          attributes[key] = firebase.firestore.FieldValue.delete();
        }
      });

      resolve(attributes);
    });
  }
}
