import {EnvironmentCdn, EnvironmentCloud} from '@deckdeckgo/sync';

export interface EnvironmentFirebaseConfig {
  apiKey: string;
  authDomain: string;
  databaseURL: string;
  storageBucket: string;
  projectId: string;
  messagingSenderId: string;
  storageUrl: string;
  functionsUrl: string;
  appId: string;
}

// Duplicate EnvironmentIC in @deckdeckgo/ic
export interface EnvironmentICConfig {
  managerCanisterId: string;
  localIdentityCanisterId?: string;
  kitPath: string;
  author: string;
}

export interface EnvironmentTenorConfig {
  url: string;
  key: string;
}

export interface EnvironmentUnsplashConfig {
  url: string;
  cdn: string;
}

export interface EnvironmentGoogleConfig {
  fontsUrl: string;
}

export interface EnvironmentDeckDeckGoConfig {
  website: string;
  globalAssetsUrl: string;
  pollUrl: string;
  apiUrl?: string;
  socketUrl: string;
  terms: string;
  privacy: string;
}

export type EnvironmentAppConfigFeature = 'doc' | 'deck';

export interface EnvironmentAppConfig {
  mock: boolean;
  features: [EnvironmentAppConfigFeature, ...EnvironmentAppConfigFeature[]];
}

export interface EnvironmentConfig {
  app: EnvironmentAppConfig;
  deckdeckgo: EnvironmentDeckDeckGoConfig;
  cloud?: EnvironmentCloud;
  jszip?: EnvironmentCdn;
  firebase?: EnvironmentFirebaseConfig;
  tenor?: EnvironmentTenorConfig;
  unsplash?: EnvironmentUnsplashConfig;
  google: EnvironmentGoogleConfig;
  ic?: EnvironmentICConfig;
}

export function setupConfig(config: EnvironmentConfig) {
  if (!window) {
    return;
  }

  const win = window as any;
  const DeckGo = win.DeckGo;

  if (DeckGo && DeckGo.config && DeckGo.config.constructor.name !== 'Object') {
    console.error('DeckDeckGo config was already initialized');
    return;
  }

  win.DeckGo = win.DeckGo || {};
  win.DeckGo.config = {
    ...win.DeckGo.config,
    ...config
  };

  return win.DeckGo.config;
}
