import {EnvironmentCloud} from './env';
import type { StyloConfig } from '@papyrs/stylo';

export interface StudioConfig {
  cloud: EnvironmentCloud | undefined;
  i18n: Record<string, Record<string, string>>;
  stylo: Partial<StyloConfig>;
}
