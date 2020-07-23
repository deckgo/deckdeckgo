export class FirestoreUtils {
  static filterDelete<T>(obj: T): T {
    if (typeof obj !== 'object' || Array.isArray(obj)) {
      return obj;
    }

    return Object.keys(obj)
      .filter((key) => !this.shouldAttributeBeCleaned(obj[key]))
      .reduce((res, key) => ((res[key] = this.filterDelete(obj[key])), res), {} as T);
  }

  static shouldAttributeBeCleaned<T>(attr: T): boolean {
    // If attr is a not an object (string, number or boolean) for sure it isn't a Firestore FieldValue.delete
    if (typeof attr !== 'object' || Array.isArray(attr)) {
      return false;
    }

    const firestoreDelete = Object.keys(attr).find((key: string) => {
      return attr[key] === 'FieldValue.delete';
    });

    return firestoreDelete !== null && firestoreDelete !== undefined;
  }
}
