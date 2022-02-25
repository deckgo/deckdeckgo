import {EnvStore} from '../stores/env.store';
import {EnvironmentCloud} from '../types/env.types';

export const cloudProvider = <T>(): Promise<T> => {
  const {api}: EnvironmentCloud = EnvStore.getInstance().get().cloud;

  const {cdn} = api;

  return import(cdn);
};
