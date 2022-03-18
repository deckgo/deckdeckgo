import {AuthUser, DeleteAuth, InitAuth, log, SignOut, User} from '@deckdeckgo/editor';
import {Identity} from '@dfinity/agent';
import {AuthClient} from '@dfinity/auth-client';
import {_SERVICE as ManagerActor} from '../../canisters/manager/manager.did';
import {EnvStore} from '../../stores/env.store';
import {EnvironmentIC} from '../../types/env.types';
import {InternetIdentityAuth} from '../../types/identity';
import {SignOutWindow} from '../../types/sync.window';
import {internetIdentityAuth} from '../../utils/identity.utils';
import {createManagerActor} from '../../utils/manager.utils';
import {startIdleTime, stopIdleTimer} from '../../workers/idle.ic.worker';
import {initUserWorker} from '../../workers/user.ic.worker';

declare global {
  interface Window {
    authClient: AuthClient | undefined;
  }
}

let authClient: AuthClient | undefined;

export const initAuth: InitAuth = async ({
  config,
  success
}: {
  config: Record<string, string | boolean>;
  success: ({authUser, user}: {authUser: AuthUser | null; user: User | undefined}) => Promise<void>;
  reset: () => Promise<void>;
}) => {
  EnvStore.getInstance().set(config as EnvironmentIC);

  authClient = await AuthClient.create();

  const isAuthenticated: boolean = (await authClient?.isAuthenticated()) || false;

  if (!isAuthenticated) {
    return;
  }

  const internetIdentity: InternetIdentityAuth = await internetIdentityAuth();

  await initUser({success});

  const onInitUserSuccess: (user: User) => Promise<void> = async (user: User) => await authenticatedUser({user, success});

  await initUserWorker({internetIdentity, env: EnvStore.getInstance().get()}, onInitUserSuccess, log);

  const onSignOut: SignOutWindow = (): void => {
    const $event: CustomEvent<void> = new CustomEvent<void>('ddgSignOut', {bubbles: true});
    document.dispatchEvent($event);
  };

  await startIdleTime({internetIdentity}, onSignOut);
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

export const signOut: SignOut = async (): Promise<void> => {
  await stopIdleTimer();

  await authClient?.logout();
};

export const signIn = async ({onSuccess, onError}: {onSuccess: () => void; onError: (err?: string) => void}) => {
  authClient = authClient || (await AuthClient.create());

  await authClient.login({
    onSuccess,
    onError,
    ...(EnvStore.getInstance().localIdentity() && {
      identityProvider: `http://localhost:8000?canisterId=${EnvStore.getInstance().get().localIdentityCanisterId}#authorize`
    })
  });
};

export const deleteAuth: DeleteAuth = async (_auth: {user: User; config}): Promise<void> => {
  const identity: Identity | undefined = getIdentity();

  if (!identity) {
    throw new Error('Invalid identity.');
  }

  const managerActor: ManagerActor = await createManagerActor({identity});
  await Promise.all([managerActor.delData(), managerActor.delStorage()]);
};

export const getIdentity = (): Identity | undefined => {
  return authClient?.getIdentity();
};
