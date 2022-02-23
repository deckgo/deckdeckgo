import {EnvironmentCloud} from '../types/env.types';

export const cloudProvider = <T>(cloud: EnvironmentCloud): Promise<T> => {
  const {api}: EnvironmentCloud = cloud;

  const {cdn} = api;

  return import(cdn);
};
