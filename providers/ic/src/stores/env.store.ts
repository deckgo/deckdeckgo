import {EnvironmentIC} from '../types/env.types';

export class EnvStore {
  private static instance: EnvStore;

  private env: EnvironmentIC | undefined;

  private constructor() {}

  static getInstance() {
    if (!EnvStore.instance) {
      EnvStore.instance = new EnvStore();
    }
    return EnvStore.instance;
  }

  set(env: EnvironmentIC) {
    this.env = env;
  }

  get(): EnvironmentIC {
    if (this.env === undefined) {
      throw new Error('No IC environment configuration set.');
    }

    return this.env;
  }

  localIdentity(): boolean {
    return this.get().localIdentityCanisterId !== undefined;
  }
}
