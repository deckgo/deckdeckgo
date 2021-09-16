import firebase from 'firebase/app';
import '@firebase/auth';
import 'firebase/firestore';

import {User as FirebaseUser} from '@firebase/auth-types';

import {ApiUser, AuthUser, User} from '@deckdeckgo/editor';
import {deleteApi, signInApi} from '@deckdeckgo/api';

import {createUser, deleteUser} from '../data/user.firebase';

export const initAuth = ({
  config,
  success,
  reset
}: {
  config: Record<string, string>;
  success: ({authUser, user, apiUser}: {authUser: AuthUser | null; user: User | undefined; apiUser?: ApiUser}) => Promise<void>;
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

        const apiUser: ApiUser | undefined = await signInAwsApi({config, authUser});

        await success({authUser, user, apiUser});
      }
    });
  } catch (err) {
    console.error(
      'Hey hi. There was an issue with the authentication for anonymous or registered users. Checkout your internet connection and browser capabilities. For example, if you are using Firefox Incognito, enable "remember history" (see issue #827 in our repo).'
    );
  }
};

const signInAwsApi = async ({config, authUser}: {config; authUser: AuthUser}): Promise<ApiUser | undefined> => {
  const token: string = await firebase.auth().currentUser.getIdToken();

  const {mock, apiUrl} = config;

  return signInApi({
    authUser,
    token,
    mock,
    apiUrl
  });
};

export const signOut = async () => {
  await firebase.auth().signOut();
};

export const signIn = async () => {
  // Do nothing, with Firebase the state of the authentication is observed, see onAuthStateChanged, and we use Firebase UI to handle login form
};

export const deleteAuth = async ({uid, apiUserId, config}: {uid: string; apiUserId: string; config}) => {
  const firebaseUser: FirebaseUser = firebase.auth().currentUser;

  if (!firebaseUser) {
    return;
  }

  // We need the user token to access the API, therefore delete it here first
  await deleteAwsApi({apiUserId, config});

  // Then delete the user
  await deleteUser(uid);

  // Decks and slides are delete with a cloud function triggered on auth.delete

  await firebaseUser.delete();
};

const deleteAwsApi = async ({apiUserId, config}: {apiUserId: string; config}) => {
  const token: string = await firebase.auth().currentUser.getIdToken();

  const {mock, apiUrl} = config;

  await deleteApi({
    userId: apiUserId,
    token,
    mock,
    apiUrl
  });
};
