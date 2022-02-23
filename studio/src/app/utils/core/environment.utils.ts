import {EnvironmentAppConfig} from '../../config/environment-config';
import {EnvironmentConfigService} from '../../services/environment/environment-config.service';

export const tenor = (): boolean => EnvironmentConfigService.getInstance().get('tenor') !== undefined;

export const unsplash = (): boolean => EnvironmentConfigService.getInstance().get('unsplash') !== undefined;

export const firebase = (): boolean => EnvironmentConfigService.getInstance().get('firebase') !== undefined;

export const cloud = (): boolean => EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('cloud') !== undefined;
