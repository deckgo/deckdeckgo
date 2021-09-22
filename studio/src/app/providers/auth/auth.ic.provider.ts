import {AuthClient} from '@dfinity/auth-client';
import {Identity} from '@dfinity/agent';

import navStore, {NavDirection} from '../../stores/nav.store';
import errorStore from '../../stores/error.store';

export class AuthIcProvider {
  private static instance: AuthIcProvider;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!AuthIcProvider.instance) {
      AuthIcProvider.instance = new AuthIcProvider();
    }
    return AuthIcProvider.instance;
  }

  private authClient: AuthClient | undefined;

  async signIn() {
    const authClient: AuthClient = this.authClient || (await AuthClient.create());

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
      ...(process.env.LOCAL_IDENTITY && {
        identityProvider: `http://localhost:8000?canisterId=${process.env.LOCAL_IDENTITY_CANISTER_ID}#authorize`
      })
    });
  }

  async signOut() {
    await this.authClient?.logout();
  }

  getIdentity(): Identity | undefined {
    return this.authClient?.getIdentity();
  }
}
