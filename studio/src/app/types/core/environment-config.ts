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

export interface EnvironmentTenorConfig {
  url: string;
  key: string;
}

export interface EnvironmentUnsplashConfig {
  url: string;
}

export interface EnvironmentGoogleConfig {
  fontsUrl: string;
}

export interface EnvironmentDeckDeckGoConfig {
  mock: boolean;
  cloud: 'offline' | 'firebase' | 'ic';
  appUrl: string;
  globalAssetsUrl: string;
  pollUrl: string;
  apiUrl?: string;
  presentationUrl: string;
  socketUrl: string;
}

export interface EnvironmentConfig {
  deckdeckgo: EnvironmentDeckDeckGoConfig;
  firebase?: EnvironmentFirebaseConfig;
  tenor?: EnvironmentTenorConfig;
  unsplash?: EnvironmentUnsplashConfig;
  google: EnvironmentGoogleConfig;
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
