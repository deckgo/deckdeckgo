import firebase from 'firebase/app';
import 'firebase/firestore';

export class FirestoreUtils {
  static filterDelete<T>(obj: T | null, replaceWithNull: boolean = false): T {
    if (!obj) {
      return obj;
    }

    if (typeof obj !== 'object' || Array.isArray(obj)) {
      return obj;
    }

    return Object.keys(obj)
      .filter((key) => !this.shouldAttributeBeCleaned(obj[key]))
      .reduce((res, key) => {
        const value: T = this.filterDelete(obj[key]);

        if (value && typeof value === 'object') {
          // We don't want to keep empty leaf {}
          if (Object.keys(value).length > 0) {
            res[key] = value;
          } else if (replaceWithNull) {
            res[key] = null;
          }
        } else {
          res[key] = value;
        }

        return res;
      }, {} as T);
  }

  static shouldAttributeBeCleaned<T>(attr: T): boolean {
    // If attr is a not an object (string, number or boolean) for sure it isn't a Firestore FieldValue.delete
    if (typeof attr !== 'object' || Array.isArray(attr)) {
      return false;
    }

    return JSON.stringify(attr) === JSON.stringify(firebase.firestore.FieldValue.delete());
  }
}
