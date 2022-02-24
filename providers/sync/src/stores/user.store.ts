import {User} from '@deckdeckgo/editor';
import {Store} from './store';

export class UserStore extends Store<User | null | undefined> {
  private static instance: UserStore;

  private user: User | undefined = undefined;

  private constructor() {
    super();
  }

  static getInstance() {
    if (!UserStore.instance) {
      UserStore.instance = new UserStore();
    }
    return UserStore.instance;
  }

  set(user: User | undefined) {
    this.user = user;

    this.populate(user);
  }

  get(): User | undefined {
    return this.user;
  }

  override subscribe(callback: (data: User | null) => void): () => void {
    const unsubscribe: () => void = super.subscribe(callback);

    callback(this.user);

    return unsubscribe;
  }

  reset() {
    this.user = undefined;

    this.populate(this.user);
  }
}
