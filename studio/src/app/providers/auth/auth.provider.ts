import {AuthUser, InitAuth, User, SignOut, SignIn, DeleteAuth} from '@deckdeckgo/editor';

import authStore from '../../stores/auth.store';
import store from '../../stores/user.store';

import {EnvironmentAppConfig, EnvironmentDeckDeckGoConfig} from '../../types/core/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

import {AuthIcProvider} from './auth.ic.provider';
import {UserIcProvider} from '../data/user/user.ic.provider';

export const initAuthProvider = async () => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  if (!['firebase', 'ic'].includes(cloud)) {
    return;
  }

  // TODO: extract IC
  if ('ic' === cloud) {
    await AuthIcProvider.getInstance().init();
    return;
  }

  const cdn: string = 'http://localhost:3335/build/index.esm.js';

  const {initAuth}: {initAuth: InitAuth} = await import(cdn);

  await initAuth({
    config: firebaseApiConfig(),
    success: onInitSuccess,
    reset: onInitReset
  });
};

const onInitReset = async () => {
  authStore.reset();
};

const onInitSuccess = async ({authUser, user}: {authUser: AuthUser | null; user: User | undefined}) => {
  authStore.state.authUser = {...authUser};

  store.state.user = {...user};
};

export const signOut = async () => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  if (!['firebase', 'ic'].includes(cloud)) {
    return;
  }

  // TODO: extract IC
  if ('ic' === cloud) {
    await AuthIcProvider.getInstance().signOut();
    return;
  }

  const cdn: string = 'http://localhost:3335/build/index.esm.js';

  const {signOut}: {signOut: SignOut} = await import(cdn);

  await signOut();
};

export const signIn = async () => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  if (!['firebase', 'ic'].includes(cloud)) {
    return;
  }

  // TODO: extract IC
  if ('ic' === cloud) {
    await AuthIcProvider.getInstance().signIn();
    return;
  }

  const cdn: string = 'http://localhost:3335/build/index.esm.js';

  const {signIn}: {signIn: SignIn} = await import(cdn);

  await signIn();
};

export const deleteAuth = async () => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  if (!['firebase', 'ic'].includes(cloud)) {
    return;
  }

  const uid: string = authStore.state.authUser.uid;

  // TODO: extract IC
  if ('ic' === cloud) {
    await UserIcProvider.getInstance().delete(uid);
    await signOut();
    return;
  }

  const cdn: string = 'http://localhost:3335/build/index.esm.js';

  const {deleteAuth}: {deleteAuth: DeleteAuth} = await import(cdn);

  await deleteAuth({uid, config: firebaseApiConfig()});
  await signOut();
};

const firebaseApiConfig = (): Record<string, string | boolean> => {
  const firebaseConfig: Record<string, string> = EnvironmentConfigService.getInstance().get('firebase');

  const {mock}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
  const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  return {
    ...firebaseConfig,
    mock,
    apiUrl: config.apiUrl
  };
};
