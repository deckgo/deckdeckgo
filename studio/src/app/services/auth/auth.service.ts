import firebase from '@firebase/app';
import '@firebase/auth';
import {User as FirebaseUser} from '@firebase/auth-types';

import errorStore from '../../stores/error.store';
import authStore from '../../stores/auth.store';

import {get, set, del} from 'idb-keyval';

import {EnvironmentConfigService} from '../core/environment/environment-config.service';

import {AuthUser} from '../../models/auth/auth.user';

import {ApiUserService} from '../api/user/api.user.service';
import {UserService} from '../data/user/user.service';
import {ApiUserFactoryService} from '../api/user/api.user.factory.service';

export class AuthService {
  private apiUserService: ApiUserService;

  private firestoreUserService: UserService;

  private static instance: AuthService;

  private constructor() {
    // Private constructor, singleton
    this.apiUserService = ApiUserFactoryService.getInstance();
    this.firestoreUserService = UserService.getInstance();
  }

  static getInstance() {
    if (!AuthService.instance) {
      AuthService.instance = new AuthService();
    }
    return AuthService.instance;
  }

  async init() {
    try {
      // We also save the user in the local storage to avoid a flickering in the GUI till Firebase as correctly fetched the user
      // And we also need it in case the user go offline, so we could also check offline if user is anonymous or not
      // TODO: maybe we do not need it anymore
      const localUser: AuthUser | undefined = await this.getLocalAuthUser();
      authStore.state.authUser = localUser ? {...localUser} : null;

      firebase.initializeApp(EnvironmentConfigService.getInstance().get('firebase'));

      firebase.auth().onAuthStateChanged(async (firebaseUser: FirebaseUser | null) => {
        if (!firebaseUser) {
          authStore.reset();
          await del('deckdeckgo_auth_user');

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

          await set('deckdeckgo_auth_user', authUser);

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

  async signOut() {
    await firebase.auth().signOut();

    await this.apiUserService.signOut();

    await del('deckdeckgo_redirect');
    await del('deckdeckgo_redirect_info');
  }

  signInAnonymous(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        await firebase.auth().signInAnonymously();

        resolve();
      } catch (err) {
        errorStore.state.error = err.message;
        resolve();
      }
    });
  }

  async getLocalAuthUser(): Promise<AuthUser | undefined> {
    return get<AuthUser>('deckdeckgo_auth_user');
  }
}
