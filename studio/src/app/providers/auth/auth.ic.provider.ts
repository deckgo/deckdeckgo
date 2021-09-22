import {AuthClient} from '@dfinity/auth-client';
import {Identity} from '@dfinity/agent';

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

  getIdentity(): Identity | undefined {
    return this.authClient?.getIdentity();
  }
}
