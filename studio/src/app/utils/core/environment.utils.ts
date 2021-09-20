import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import {EnvironmentDeckDeckGoConfig, EnvironmentAppConfig} from '../../types/core/environment-config';

export const tenor = (): boolean => EnvironmentConfigService.getInstance().get('tenor') !== undefined;

export const unsplash = (): boolean => EnvironmentConfigService.getInstance().get('unsplash') !== undefined;

export const share = (): boolean => {
  return (
    EnvironmentConfigService.getInstance().get<EnvironmentDeckDeckGoConfig>('deckdeckgo').apiUrl !== undefined ||
    EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app').mock
  );
};

export const firebase = (): boolean => EnvironmentConfigService.getInstance().get('firebase') !== undefined;

export const internetComputer = (): boolean => EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app')?.cloud === 'ic';

export const cloud = (): boolean => EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app')?.cloud !== 'offline';
