import {SlideAttributes} from '../../models/data/slide';
import {DeckAttributes} from '../../models/data/deck';

export class OfflineUtils {
  static async cleanAttributes(attributes: SlideAttributes | DeckAttributes): Promise<SlideAttributes | DeckAttributes> {
    if (!attributes) {
      return attributes;
    }

    const keys: string[] = Object.keys(attributes);

    if (!keys || keys.length <= 0) {
      return attributes;
    }

    keys.forEach((key: string) => {
      const attr = attributes[key];

      // Replace Firestore "to delete fields" with null values
      if (this.shouldAttributeBeCleaned(attr)) {
        attributes[key] = null;
      }
    });

    return attributes;
  }

  static shouldAttributeBeCleaned(attr): any {
    return attr && attr._methodName && attr._methodName === 'FieldValue.delete';
  }
}
