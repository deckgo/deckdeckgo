import firebase from 'firebase/app';
import '@firebase/auth';
import 'firebase/firestore';

import {User as FirebaseUser} from '@firebase/auth-types';

import {AuthUser, User} from '@deckdeckgo/editor';

import {createUser, deleteUser} from './user.firebase.provider';

export const initAuth = ({
  config,
  success,
  reset
}: {
  config: Record<string, string>;
  success: ({authUser, user}: {authUser: AuthUser | null; user: User | undefined}) => Promise<void>;
  reset: () => Promise<void>;
}) => {
  try {
    firebase.initializeApp(config);

    firebase.auth().onAuthStateChanged(async (firebaseUser: FirebaseUser | null) => {
      if (!firebaseUser) {
        await reset();
      } else {
        const authUser: AuthUser = {
          uid: firebaseUser.uid,
          anonymous: firebaseUser.isAnonymous,
          name: firebaseUser.displayName,
          email: firebaseUser.email,
          email_verified: firebaseUser.emailVerified,
          photo_url: firebaseUser.photoURL,
          gitHub:
            firebaseUser.providerData && firebaseUser.providerData.length > 0
              ? firebaseUser.providerData[0].providerId === 'github.com'
              : false
        };

        // Update anonymous user
        // Reference: https://github.com/firebase/firebaseui-web/issues/449
        if (
          !authUser.name &&
          firebaseUser.providerData &&
          firebaseUser.providerData.length > 0 &&
          firebaseUser.providerData[0].displayName
        ) {
          authUser.name = firebaseUser.providerData[0].displayName;
        }

        if (
          !authUser.photo_url &&
          firebaseUser.providerData &&
          firebaseUser.providerData.length > 0 &&
          firebaseUser.providerData[0].photoURL
        ) {
          authUser.photo_url = firebaseUser.providerData[0].photoURL;
        }

        const user: User = await createUser(authUser);

        await success({authUser, user});
      }
    });
  } catch (err) {
    console.error(
      'Hey hi. There was an issue with the authentication for anonymous or registered users. Checkout your internet connection and browser capabilities. For example, if you are using Firefox Incognito, enable "remember history" (see issue #827 in our repo).'
    );
  }
};

export const signOut = async ({success}: {success: () => Promise<void>}) => {
  await firebase.auth().signOut();

  await success();
};

export const signIn = async () => {
  // Do nothing, with Firebase the state of the authentication is observed, see onAuthStateChanged, and we use Firebase UI to handle login form
};

export const deleteAuth = async ({uid, preDelete}: {uid: string; preDelete: (token: string) => Promise<void>}) => {
  const firebaseUser: FirebaseUser = firebase.auth().currentUser;

  if (!firebaseUser) {
    return;
  }

  // We need the user token to access the API, therefore delete it here first
  const token: string = await firebase.auth().currentUser.getIdToken();
  await preDelete(token);

  // Then delete the user
  await deleteUser(uid);

  // Decks and slides are delete with a cloud function triggered on auth.delete

  await firebaseUser.delete();
};
