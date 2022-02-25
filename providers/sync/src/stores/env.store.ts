import {Environment} from '../types/env.types';
import {Store} from './store';

export class EnvStore extends Store<Environment | undefined> {
  private static instance: EnvStore;

  private env: Environment | undefined;

  private constructor() {
    super();
  }

  static getInstance() {
    if (!EnvStore.instance) {
      EnvStore.instance = new EnvStore();
    }
    return EnvStore.instance;
  }

  set(env: Environment | undefined) {
    this.env = env;

    this.populate(env);
  }

  get(): Environment | undefined {
    return this.env;
  }

  cloud(): boolean {
    return this.env?.cloud !== undefined;
  }

  override subscribe(callback: (data: Environment | undefined) => void): () => void {
    const unsubscribe: () => void = super.subscribe(callback);

    callback(this.env);

    return unsubscribe;
  }
}
