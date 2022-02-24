import {AuthUser, DeleteAuth, InitAuth, SignOut, User} from '@deckdeckgo/editor';
import {AuthStore} from '../stores/auth.store';
import {EnvStore} from '../stores/env.store';
import {UserStore} from '../stores/user.store';
import {cloudProvider} from '../utils/providers.utils';

export const initAuthProvider = async (config: Record<string, string | boolean> = {}) => {
  if (!EnvStore.getInstance().cloud()) {
    return;
  }

  const {initAuth}: {initAuth: InitAuth} = await cloudProvider<{initAuth: InitAuth}>();

  await initAuth({
    config,
    success: onInitSuccess,
    reset: onInitReset
  });
};

const onInitReset = async () => AuthStore.getInstance().reset();

const onInitSuccess = async ({authUser, user}: {authUser: AuthUser | null; user: User | undefined}) => {
  AuthStore.getInstance().set({...authUser});

  UserStore.getInstance().set({...user});
};

export const signOut = async () => {
  if (!EnvStore.getInstance().cloud()) {
    return;
  }

  const {signOut}: {signOut: SignOut} = await cloudProvider<{signOut: SignOut}>();

  await signOut();
};

export const deleteAuth = async (config: Record<string, string | boolean> = {}) => {
  if (!EnvStore.getInstance().cloud()) {
    return;
  }

  const {deleteAuth}: {deleteAuth: DeleteAuth} = await cloudProvider<{deleteAuth: DeleteAuth}>();

  await deleteAuth({user: UserStore.getInstance().get(), config});
  await signOut();
};
