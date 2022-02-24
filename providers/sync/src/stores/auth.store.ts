import {AuthUser} from '@deckdeckgo/editor';
import {Store} from './store';

export class AuthStore extends Store<AuthUser | null> {
  private static instance: AuthStore;

  private authUser: AuthUser | null = null;

  private constructor() {
    super();
  }

  static getInstance() {
    if (!AuthStore.instance) {
      AuthStore.instance = new AuthStore();
    }
    return AuthStore.instance;
  }

  set(authUser: AuthUser | null) {
    this.authUser = authUser;

    this.populate(authUser);
  }

  get(): AuthUser | null {
    return this.authUser;
  }

  override subscribe(callback: (data: AuthUser | null) => void): () => void {
    const unsubscribe: () => void = super.subscribe(callback);

    callback(this.authUser);

    return unsubscribe;
  }

  isLoggedIn(): boolean {
    return this.authUser?.state === 'authenticated';
  }

  reset() {
    this.authUser = null;
  }
}
