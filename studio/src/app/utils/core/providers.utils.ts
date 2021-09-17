import {EnvironmentCloud} from '../../types/core/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

export const provider = <T>(): Promise<T> => {
  const {cdn}: EnvironmentCloud = EnvironmentConfigService.getInstance().get('cloud');

  return import(cdn);
};
