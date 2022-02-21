import type {StyloConfig} from '@papyrs/stylo';
import {EnvironmentCloud} from './env';
import {Languages} from './i18n';

export interface StudioConfig {
  cloud: EnvironmentCloud | undefined;
  lang: Languages;
  stylo: Partial<StyloConfig>;
}
