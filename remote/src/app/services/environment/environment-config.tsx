export interface EnvironmentConfig {
    signalingServerUrl: string;
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
