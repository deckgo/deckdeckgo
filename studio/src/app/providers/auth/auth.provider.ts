import {AuthUser, InitAuth, User, SignOut, DeleteAuth} from '@deckdeckgo/editor';

import authStore from '../../stores/auth.store';
import userStore from '../../stores/user.store';

import {EnvironmentAppConfig, EnvironmentDeckDeckGoConfig} from '../../types/core/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

import {UserIcProvider} from '../data/user/user.ic.provider';

import {cloudProvider} from '../../utils/core/providers.utils';

export const initAuthProvider = async () => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  if (!['firebase', 'ic'].includes(cloud)) {
    return;
  }

  const {initAuth}: {initAuth: InitAuth} = await cloudProvider<{initAuth: InitAuth}>();

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

  userStore.state.user = {...user};
};

export const signOut = async () => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  if (!['firebase', 'ic'].includes(cloud)) {
    return;
  }

  const {signOut}: {signOut: SignOut} = await cloudProvider<{signOut: SignOut}>();

  await signOut();
};

export const deleteAuth = async () => {
  const {cloud}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');

  if (!['firebase', 'ic'].includes(cloud)) {
    return;
  }

  // TODO: extract IC
  if ('ic' === cloud) {
    const uid: string = authStore.state.authUser.uid;
    await UserIcProvider.getInstance().delete(uid);

    await signOut();
    return;
  }

  const {deleteAuth}: {deleteAuth: DeleteAuth} = await cloudProvider<{deleteAuth: DeleteAuth}>();

  await deleteAuth({user: userStore.state.user, config: firebaseApiConfig()});
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
