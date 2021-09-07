import firebase from 'firebase/app';
import 'firebase/firestore';

import errorStore from '../../../stores/error.store';

import {Token} from '../../../models/data/token';

export class PlatformService {
  private static instance: PlatformService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!PlatformService.instance) {
      PlatformService.instance = new PlatformService();
    }
    return PlatformService.instance;
  }

  async merge(token: Token) {
    if (!token) {
      return;
    }

    try {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      token.data.updated_at = firebase.firestore.Timestamp.now();

      await firestore.collection('tokens').doc(token.id).set(token.data, {merge: true});
    } catch (err) {
      errorStore.state.error = 'GitHub platform information not properly set up.';
    }
  }
}
