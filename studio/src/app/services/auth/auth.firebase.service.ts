import firebase from '@firebase/app';
import '@firebase/auth';
import {User as FirebaseUser} from '@firebase/auth-types';

import authStore from '../../stores/auth.store';

import {del} from 'idb-keyval';

import {EnvironmentConfigService} from '../environment/environment-config.service';

import {AuthUser} from '../../models/auth/auth.user';

import {ApiUserService} from '../api/user/api.user.service';
import {UserService} from '../data/user/user.service';
import {ApiUserFactoryService} from '../api/user/api.user.factory.service';
import {AuthService} from './auth.service';

export class AuthFirebaseService extends AuthService {
  private apiUserService: ApiUserService;

  private firestoreUserService: UserService;

  constructor() {
    super();

    this.apiUserService = ApiUserFactoryService.getInstance();
    this.firestoreUserService = UserService.getInstance();
  }

  // @Override
  async init() {
    try {
      firebase.initializeApp(EnvironmentConfigService.getInstance().get('firebase'));

      firebase.auth().onAuthStateChanged(async (firebaseUser: FirebaseUser | null) => {
        if (!firebaseUser) {
          authStore.reset();
          await this.apiUserService.signOut();
        } else {
          const authUser: AuthUser = {
            uid: firebaseUser.uid,
            anonymous: firebaseUser.isAnonymous,
            name: firebaseUser.displayName,
            email: firebaseUser.email,
            email_verified: firebaseUser.emailVerified,
            photo_url: firebaseUser.photoURL,
            gitHub: firebaseUser.providerData && firebaseUser.providerData.length > 0 ? firebaseUser.providerData[0].providerId === 'github.com' : false
          };

          // Update anonymous user
          // Reference: https://github.com/firebase/firebaseui-web/issues/449
          if (!authUser.name && firebaseUser.providerData && firebaseUser.providerData.length > 0 && firebaseUser.providerData[0].displayName) {
            authUser.name = firebaseUser.providerData[0].displayName;
          }

          if (!authUser.photo_url && firebaseUser.providerData && firebaseUser.providerData.length > 0 && firebaseUser.providerData[0].photoURL) {
            authUser.photo_url = firebaseUser.providerData[0].photoURL;
          }

          await this.firestoreUserService.create(authUser);

          authStore.state.authUser = {...authUser};

          await this.apiUserService.signIn(authUser);
        }
      });
    } catch (err) {
      console.error(
        'Hey hi. There was an issue with the authentication for anonymous or registered users. Checkout your internet connection and browser capabilities. For example, if you are using Firefox Incognito, enable "remember history" (see issue #827 in our repo).'
      );
    }
  }

  // @Override
  async signIn() {
    // Do nothing, with Firebase the state of the authentication is observed, see onAuthStateChanged, and we use Firebase UI to handle login form
  }

  // @Override
  async signOut() {
    await firebase.auth().signOut();

    await this.apiUserService.signOut();

    await del('deckdeckgo_redirect');
    await del('deckdeckgo_redirect_info');
  }
}
