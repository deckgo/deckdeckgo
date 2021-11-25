import {AuthClient} from '@dfinity/auth-client';
import {Identity} from '@dfinity/agent';

import {AuthUser, InitAuth, SignOut, User, DeleteAuth} from '@deckdeckgo/editor';

import {InternetIdentityAuth} from '../../types/identity';

import {internetIdentityAuth} from '../../utils/identity.utils';
import {initUserWorker} from '../../workers/user.ic.worker';
import {deleteUser} from '../data/user.ic';

declare global {
  interface Window {
    authClient: AuthClient | undefined;
  }
}

let authClient: AuthClient | undefined;

export const initAuth: InitAuth = async ({
  success
}: {
  config: Record<string, string | boolean>;
  success: ({authUser, user}: {authUser: AuthUser | null; user: User | undefined}) => Promise<void>;
  reset: () => Promise<void>;
}) => {
  authClient = await AuthClient.create();

  const isAuthenticated: boolean = (await authClient?.isAuthenticated()) || false;

  if (!isAuthenticated) {
    return;
  }

  const internetIdentity: InternetIdentityAuth = await internetIdentityAuth();

  await initUser({success});

  const user: User = await initUserWorker({internetIdentity, host: `https://ic0.app`});

  await authenticatedUser({user, success});
};

// If first sign-in, initializing the canister can take a while therefore we already emit a not fully authenticated user
const initUser = async ({success}: {success: ({authUser, user}: {authUser: AuthUser | null; user: User | undefined}) => Promise<void>}) => {
  const authUser: AuthUser = {
    state: 'initialization'
  } as AuthUser;

  await success({authUser, user: undefined});
};

const authenticatedUser = async ({
  user,
  success
}: {
  user: User;
  success: ({authUser, user}: {authUser: AuthUser | null; user: User | undefined}) => Promise<void>;
}) => {
  const {id, data} = user;

  const {name, email, photo_url} = data;

  const authUser: AuthUser = {
    uid: id,
    state: 'authenticated',
    name,
    email,
    photo_url
  } as AuthUser;

  await success({authUser, user});
};

export const signOut: SignOut = (): Promise<void> => {
  return authClient?.logout();
};

export const signIn = async ({onSuccess, onError}: {onSuccess: () => void; onError: (err?: string) => void}) => {
  authClient = authClient || (await AuthClient.create());

  await authClient.login({
    onSuccess,
    onError,
    ...(process.env.LOCAL_IDENTITY && {
      identityProvider: `http://localhost:8000?canisterId=${process.env.LOCAL_IDENTITY_CANISTER_ID}#authorize`
    })
  });
};

export const deleteAuth: DeleteAuth = ({user}: {user: User; config}): Promise<void> => {
  return deleteUser(user.id);
};

export const getIdentity = (): Identity | undefined => {
  return authClient?.getIdentity();
};
