import {firebase} from '@firebase/app';

import {SlideAttributes} from '../../models/data/slide';
import {DeckAttributes} from '../../models/data/deck';

export class OfflineUtils {
  static cleanAttributes(attributes: SlideAttributes | DeckAttributes): Promise<SlideAttributes | DeckAttributes> {
    return new Promise<SlideAttributes | DeckAttributes>((resolve) => {
      if (!attributes) {
        resolve(attributes);
        return;
      }

      const keys: string[] = Object.keys(attributes);

      if (!keys || keys.length <= 0) {
        resolve(attributes);
        return;
      }

      keys.forEach((key: string) => {
        const attr = attributes[key];

        // Replace Firestore "to delete fields" with null values
        if (this.shouldAttributeBeCleaned(attr)) {
          attributes[key] = null;
        }
      });

      resolve(attributes);
    });
  }

  static shouldAttributeBeCleaned(attr): any {
    return attr && attr._methodName && attr._methodName === 'FieldValue.delete';
  }

  static prepareAttributes(attributes: SlideAttributes | DeckAttributes): Promise<SlideAttributes | DeckAttributes> {
    return new Promise<SlideAttributes | DeckAttributes>((resolve) => {
      if (!attributes) {
        resolve(attributes);
        return;
      }

      const keys: string[] = Object.keys(attributes);

      if (!keys || keys.length <= 0) {
        resolve(attributes);
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
