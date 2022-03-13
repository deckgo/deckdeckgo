import {EnvironmentAppConfig, EnvironmentDeckDeckGoConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

export const authConfig = (): Record<string, string | boolean> => {
  const icConfig: Record<string, string> | undefined = EnvironmentConfigService.getInstance().get('ic') || {};

  return {
    ...firebaseApiConfig(),
    ...icConfig
  };
};

const firebaseApiConfig = (): Record<string, string | boolean> => {
  const firebaseConfig: Record<string, string> = EnvironmentConfigService.getInstance().get('firebase') || {};

  const {mock}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
  const {apiUrl}: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  return {
    ...firebaseConfig,
    mock,
    ...(apiUrl && {apiUrl})
  };
};
