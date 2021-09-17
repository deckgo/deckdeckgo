import firebase from 'firebase/app';
import 'firebase/firestore';

export const filterFieldDelete = <T>(obj: T | null, replaceWithNull: boolean = false): T | null => {
  if (!obj) {
    return obj;
  }

  if (typeof obj !== 'object' || Array.isArray(obj)) {
    return obj;
  }

  return Object.keys(obj)
    .filter((key) => !shouldAttributeBeCleaned(obj[key]))
    .reduce((res, key) => {
      const value: T = filterFieldDelete(obj[key]);

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
};

const shouldAttributeBeCleaned = <T>(attr: T): boolean => {
  // If attr is a not an object (string, number or boolean) for sure it isn't a Firestore FieldValue.delete
  if (typeof attr !== 'object' || Array.isArray(attr)) {
    return false;
  }

  return JSON.stringify(attr) === JSON.stringify(firebase.firestore.FieldValue.delete());
};

export const prepareAttributes = <T>(attributes: T): T => {
  if (!attributes) {
    // @ts-ignore
    return firebase.firestore.FieldValue.delete();
  }

  const keys: string[] = Object.keys(attributes);

  if (!keys || keys.length <= 0) {
    // @ts-ignore
    return firebase.firestore.FieldValue.delete();
  }

  // We use a counter to spare an iteration
  let keysToDelete: number = 0;

  keys.forEach((key: string) => {
    const attr = attributes[key];

    // Replace null values with Firestore "to delete fields"
    if (attr === null) {
      // @ts-ignore
      attributes[key] = firebase.firestore.FieldValue.delete();

      keysToDelete++;
    }
  });

  // If all keys are to be deleted, then we can delete the node itself
  if (keysToDelete === keys.length) {
    // @ts-ignore
    return firebase.firestore.FieldValue.delete();
  }

  return attributes;
};
