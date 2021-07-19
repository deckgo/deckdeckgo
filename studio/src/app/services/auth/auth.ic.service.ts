import {AuthClient} from '@dfinity/auth-client';
import type {Principal} from '@dfinity/principal';

import authStore from '../../stores/auth.store';

import {del} from 'idb-keyval';

import {AuthUser} from '../../models/auth/auth.user';

import {AuthService} from './auth.service';

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
      uid: principal.toString(),
      anonymous: false,
      gitHub: false
    } as AuthUser;
  }

  // @Override
  async signIn() {
    const authClient: AuthClient = this.authClient || (await AuthClient.create());

    // TODO: onSuccess and onError
    await authClient.login({
      onSuccess: () => {
        console.log('**** AUTH 1 **** authClient now has an identity');
      },
      onError: (err?: string) => {
        console.log('**** AUTH 2 **** error while login in', err);
      }
    });
  }

  // @Override
  async signOut() {
    await this.authClient?.logout();

    await del('deckdeckgo_redirect');
    await del('deckdeckgo_redirect_info');
  }
}
