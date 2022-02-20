import {EnvironmentCloud} from '../types/env';

export const cloudProvider = <T>(cloud: EnvironmentCloud): Promise<T> => {
  const {api}: EnvironmentCloud = cloud;

  const {cdn} = api;

  return import(cdn);
};
