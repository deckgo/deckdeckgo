import {EnvironmentCloud, EnvironmentUnsplashConfig} from '../../types/core/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

export const cloudProvider = <T>(): Promise<T> => {
  const {api}: EnvironmentCloud = EnvironmentConfigService.getInstance().get('cloud');

  const {cdn} = api;

  return import(cdn);
};

export const unsplashProvider = <T>(): Promise<T> => {
  const {cdn}: EnvironmentUnsplashConfig = EnvironmentConfigService.getInstance().get('unsplash');

  return import(cdn);
};
