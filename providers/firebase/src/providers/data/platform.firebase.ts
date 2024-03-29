import firebase from 'firebase/app';
import 'firebase/firestore';

import {Token, MergeToken} from '@deckdeckgo/editor';

export const mergeToken: MergeToken = async (token: Token) => {
  if (!token) {
    return;
  }

  const firestore: firebase.firestore.Firestore = firebase.firestore();

  token.data.updated_at = firebase.firestore.Timestamp.now() as unknown as Date;

  await firestore.collection('tokens').doc(token.id).set(token.data, {merge: true});
};
