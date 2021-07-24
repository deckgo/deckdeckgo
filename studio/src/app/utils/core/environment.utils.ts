import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import {EnvironmentDeckDeckGoConfig, EnvironmentAppConfig} from '../../types/core/environment-config';

export const tenor = (): boolean => EnvironmentConfigService.getInstance().get('tenor') !== undefined;

export const unsplash = (): boolean => {
  return (
    EnvironmentConfigService.getInstance().get('unsplash') !== undefined ||
    EnvironmentConfigService.getInstance().get<EnvironmentDeckDeckGoConfig>('deckdeckgo').mock
  );
};

export const share = (): boolean => {
  const deckdeckgoConfig = EnvironmentConfigService.getInstance().get<EnvironmentDeckDeckGoConfig>('deckdeckgo');
  return deckdeckgoConfig.apiUrl !== undefined || deckdeckgoConfig.mock;
};

export const firebase = (): boolean => EnvironmentConfigService.getInstance().get('firebase') !== undefined;

export const internetComputer = (): boolean => EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app')?.cloud === 'ic';
