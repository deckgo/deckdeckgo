import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import {EnvironmentDeckDeckGoConfig} from '../../types/core/environment-config';

export const tenorEnabled = (): boolean => EnvironmentConfigService.getInstance().get('tenor') !== undefined;

export const unsplashEnabled = (): boolean => {
  return (
    EnvironmentConfigService.getInstance().get('unsplash') !== undefined ||
    EnvironmentConfigService.getInstance().get<EnvironmentDeckDeckGoConfig>('deckdeckgo').mock
  );
};

export const shareEnabled = (): boolean => {
  const deckdeckgoConfig = EnvironmentConfigService.getInstance().get<EnvironmentDeckDeckGoConfig>('deckdeckgo');
  return deckdeckgoConfig.apiUrl !== undefined || deckdeckgoConfig.mock;
};

export const firebaseEnabled = (): boolean => EnvironmentConfigService.getInstance().get('firebase') !== undefined;
