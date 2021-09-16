import {ApiUser, AuthUser, User} from '@deckdeckgo/editor';

import authStore from '../../stores/auth.store';
import store from '../../stores/user.store';
import apiUserStore from '../../stores/api.user.store';

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

  console.log('0');

  const cdn: string = 'http://localhost:3335/build/index.esm.js';

  const {initAuth} = await import(cdn);

  console.log('1', initAuth);

  // TODO: interface
  await initAuth({
    config: firebaseApiConfig(),
    success: onInitSuccess,
    reset: onInitReset
  });

  console.log('2');
};

const onInitReset = async () => {
  authStore.reset();
  apiUserStore.reset();
};

const onInitSuccess = async ({authUser, user, apiUser}: {authUser: AuthUser | null; user: User | undefined; apiUser?: ApiUser}) => {
  authStore.state.authUser = {...authUser};

  store.state.user = {...user};

  apiUserStore.state.apiUser = apiUser ? {...apiUser} : undefined;
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

  const {signOut} = await import(cdn);

  console.log('4', signOut);

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

  const {signIn} = await import(cdn);

  console.log('5', signIn);

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

  const {deleteAuth} = await import(cdn);

  await deleteAuth({uid, apiUserId: apiUserStore.state.apiUser.id, config: firebaseApiConfig()});
  await signOut();
};

const firebaseApiConfig = () => {
  const firebaseConfig: Record<string, string> = EnvironmentConfigService.getInstance().get('firebase');

  const {mock}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
  const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  return {
    ...firebaseConfig,
    mock,
    apiUrl: config.apiUrl
  };
};
