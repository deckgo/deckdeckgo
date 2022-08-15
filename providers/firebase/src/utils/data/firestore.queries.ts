import firebase from 'firebase/app';
import 'firebase/firestore';

import {filterFieldDelete} from './firestore.utils';

import {del} from 'idb-keyval';

export const entries = <T>({userId, collection}: {userId: string; collection: string}): Promise<{id: string; data: T}[]> => {
  return new Promise<{id: string; data: T}[]>(async (resolve, reject) => {
    try {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      const snapshot: firebase.firestore.QuerySnapshot = await firestore
        .collection(collection)
        .where('owner_id', '==', userId)
        .orderBy('updated_at', 'desc')
        .get();

      const results: {id: string; data: T}[] = snapshot.docs.map((documentSnapshot: firebase.firestore.QueryDocumentSnapshot<T>) => {
        return {
          id: documentSnapshot.id,
          data: documentSnapshot.data()
        };
      });

      resolve(results);
    } catch (err) {
      reject(err);
    }
  });
};

export const deleteEntry = ({id, collection}: {id: string; collection: string}): Promise<void> => {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      await firestore.collection(collection).doc(id).delete();

      try {
        const key: string = `/${collection}/${id}`.replace(/\/\//g, '/');

        await del(key);
      } catch (err) {
        // Ignore error, in the cloud the entity has been removed and locally the data have been wiped out too
        console.error(err);
      }

      resolve();
    } catch (err) {
      reject(err);
    }
  });
};

export const createEntry = <T>({data, collection}: {data: T; collection: string}): Promise<{id: string; data: T}> => {
  return new Promise<{id: string; data: T}>(async (resolve, reject) => {
    const firestore: firebase.firestore.Firestore = firebase.firestore();

    const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
    data['created_at'] = now as unknown as Date;
    data['updated_at'] = now as unknown as Date;

    firestore
      .collection(collection)
      .add(data)
      .then(
        (doc: firebase.firestore.DocumentReference<T>) => {
          resolve({
            id: doc.id,
            data
          });
        },
        (err) => {
          reject(err);
        }
      );
  });
};

export const getEntry = <T>({id, collection}: {id: string; collection: string}): Promise<{id: string; data: T}> => {
  return new Promise<{id: string; data: T}>(async (resolve, reject) => {
    const firestore: firebase.firestore.Firestore = firebase.firestore();

    try {
      const snapshot: firebase.firestore.DocumentSnapshot = await firestore.collection(collection).doc(id).get();

      if (!snapshot.exists) {
        reject('Entry not found');
        return;
      }

      const data: T = snapshot.data() as T;

      resolve({
        id: snapshot.id,
        data
      });
    } catch (err) {
      reject(err);
    }
  });
};

export const updateEntry = <T>({entry, collection}: {entry: T; collection: string}): Promise<T> => {
  return new Promise<T>(async (resolve, reject) => {
    const firestore: firebase.firestore.Firestore = firebase.firestore();

    const now: firebase.firestore.Timestamp = firebase.firestore.Timestamp.now();
    entry['data'].updated_at = now as unknown as Date;

    try {
      await firestore.collection(collection).doc(entry['id']).set(entry['data'], {merge: true});

      resolve(filterFieldDelete<T>(entry));
    } catch (err) {
      reject(err);
    }
  });
};

export const snapshotEntry = async <T>({
  id,
  collection,
  onNext,
  onError
}: {
  id: string;
  collection: string;
  onNext: (snapshot: {id: string; data: T}) => Promise<void>;
  onError?: (error: string) => void;
}): Promise<() => void | undefined> => {
  if (!id) {
    throw new Error('Id not provided');
  }

  const firestore: firebase.firestore.Firestore = firebase.firestore();

  const unsubscribe: () => void | undefined = firestore
    .collection(collection)
    .doc(id)
    .onSnapshot(
      async (deploySnapshot: firebase.firestore.DocumentSnapshot<T>) =>
        await onNext({
          id: deploySnapshot.id,
          data: deploySnapshot.data()
        }),
      ({message}: firebase.firestore.FirestoreError) => onError(message)
    );

  return unsubscribe;
};
