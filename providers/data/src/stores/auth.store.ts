import {AuthUser} from '@deckdeckgo/editor';
import {Store} from './store';

export class AuthStore extends Store<AuthUser | null | undefined> {
  private static instance: AuthStore;

  private authUser: AuthUser | null | undefined = undefined;

  private constructor() {
    super();
  }

  static getInstance() {
    if (!AuthStore.instance) {
      AuthStore.instance = new AuthStore();
    }
    return AuthStore.instance;
  }

  set(authUser: AuthUser | null | undefined) {
    this.authUser = authUser;

    this.populate(authUser);
  }

  get(): AuthUser | null | undefined {
    return this.authUser;
  }

  isLoggedIn(): boolean {
    return this.authUser?.state === 'authenticated';
  }
}
