import * as firebase from 'firebase/app';
import 'firebase/firestore';

import errorStore from '../../../stores/error.store';

import {Platform} from '../../../models/data/platform';

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

  async merge(platform: Platform) {
    if (!platform) {
      return;
    }

    try {
      const firestore: firebase.firestore.Firestore = firebase.firestore();

      platform.data.updated_at = firebase.firestore.Timestamp.now();

      await firestore.collection('platforms').doc(platform.id).set(platform.data, {merge: true});
    } catch (err) {
      errorStore.state.error = 'GitHub platform information not properly set up.';
    }
  }
}
