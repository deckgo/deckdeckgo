import {AuthUser, DeleteAuth, InitAuth, SignOut, User} from '@deckdeckgo/editor';
import {EnvironmentAppConfig, EnvironmentDeckDeckGoConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import userStore from '../../stores/user.store';
import {cloud} from '../../utils/core/environment.utils';
import {cloudProvider} from '../../utils/core/providers.utils';
import authStore from '../../stores/auth.store';

export const initAuthProvider = async () => {
  if (!cloud()) {
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
  if (!cloud()) {
    return;
  }

  const {signOut}: {signOut: SignOut} = await cloudProvider<{signOut: SignOut}>();

  await signOut();
};

export const deleteAuth = async () => {
  if (!cloud()) {
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
