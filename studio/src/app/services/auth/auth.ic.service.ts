import {AuthClient} from '@dfinity/auth-client';
import type {Principal} from '@dfinity/principal';
import {Identity} from '@dfinity/agent';

import authStore from '../../stores/auth.store';
import userStore from '../../stores/user.store';
import navStore, {NavDirection} from '../../stores/nav.store';
import errorStore from '../../stores/error.store';

import {del} from 'idb-keyval';

import {AuthUser} from '../../models/auth/auth.user';

import {AuthService} from './auth.service';
import {EnvironmentConfigService} from '../environment/environment-config.service';
import {EnvironmentAppConfig} from '../../types/core/environment-config';

export class AuthIcService extends AuthService {
  private authClient: AuthClient | undefined;

  // @Override
  async init() {
    this.authClient = await AuthClient.create();
    const isAuthenticated: boolean = (await this.authClient?.isAuthenticated()) || false;

    if (!isAuthenticated) {
      return;
    }

    const principal: Principal = this.authClient.getIdentity().getPrincipal();

    // TODO: should we or not use the principal as uid?
    authStore.state.authUser = {
      uid: principal.toText(),
      anonymous: false,
      gitHub: false
    } as AuthUser;

    // TODO: do we need a user with internet computer? probably linked with above answer too
    const now: Date = new Date();

    userStore.state.user = {
      id: principal.toText(),
      data: {
        anonymous: false,
        created_at: now,
        updated_at: now
      }
    };
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
      ...(mock && {identityProvider: `http://localhost:8000?canisterId=${process.env.__CANDID_UI_CANISTER_ID}#authorize`})
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
