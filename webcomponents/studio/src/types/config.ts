import {EnvironmentCloud} from './env';

export interface StudioConfig {
  cloud: EnvironmentCloud | undefined;
  i18n: Record<string, Record<string, string>>;
}
