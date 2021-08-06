import {AuthClient} from '@dfinity/auth-client';
import {Identity} from '@dfinity/agent';

import navStore, {NavDirection} from '../../stores/nav.store';
import errorStore from '../../stores/error.store';

import {del} from 'idb-keyval';

import {AuthService} from './auth.service';
import {EnvironmentConfigService} from '../environment/environment-config.service';
import {EnvironmentAppConfig} from '../../types/core/environment-config';
import {UserIcService} from '../data/user/user.ic.service';

export class AuthIcService extends AuthService {
  private authClient: AuthClient | undefined;

  // @Override
  async init() {
    this.authClient = await AuthClient.create();
    const isAuthenticated: boolean = (await this.authClient?.isAuthenticated()) || false;

    if (!isAuthenticated) {
      return;
    }

    await UserIcService.getInstance().create({identity: this.authClient.getIdentity()});
  }

  // @Override
  async signIn() {
    const authClient: AuthClient = this.authClient || (await AuthClient.create());

    const {mock} = EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app');

    await authClient.login({
      onSuccess: () => {
        navStore.state.nav = {
          url: '/',
          direction: NavDirection.RELOAD
        };
      },
      onError: (err?: string) => {
        console.error(err);

        errorStore.state.error = 'There was an issue sign in with the internet identity.';
      },
      ...(mock && {identityProvider: `http://localhost:8000?canisterId=${process.env.LOCAL_IDENTITY_CANISTER_ID}#authorize`})
    });
  }

  // @Override
  async signOut() {
    await this.authClient?.logout();

    await del('deckdeckgo_redirect');
    await del('deckdeckgo_redirect_info');
  }

  getIdentity(): Identity | undefined {
    return this.authClient?.getIdentity();
  }
}
