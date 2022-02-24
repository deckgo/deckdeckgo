import {EnvironmentAppConfig, EnvironmentDeckDeckGoConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

export const firebaseApiConfig = (): Record<string, string | boolean> => {
  const firebaseConfig: Record<string, string> = EnvironmentConfigService.getInstance().get('firebase');

  const {mock}: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
  const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  return {
    ...firebaseConfig,
    mock,
    apiUrl: config.apiUrl
  };
};
