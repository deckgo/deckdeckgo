import {AuthClient} from '@dfinity/auth-client';

import {AuthUser, InitAuth, SignOut, SignIn, User, DeleteAuth} from '@deckdeckgo/editor';

import {InternetIdentityAuth} from '../../types/identity';

import {internetIdentityAuth} from '../../utils/identity.utils';
import {initUserWorker} from '../../workers/user.ic.worker';

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

  const user: User = await initUserWorker({internetIdentity, host: `${window.location}`});

  await populateUser({user, success});
};

const populateUser = async ({
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
    anonymous: false,
    gitHub: false,
    name,
    email,
    photo_url
  } as AuthUser;

  await success({authUser, user});
};

export const signOut: SignOut = async () => {
  // TODO
};

export const signIn: SignIn = async () => {
  // TODO
};

export const deleteAuth: DeleteAuth = async (_param: {user: User; config}) => {
  // TODO
};
