import {EnvironmentConfig} from '../../../types/core/environment-config';

export class EnvironmentConfigService {
  private static instance: EnvironmentConfigService;

  private m: Map<keyof EnvironmentConfig, any>;

  private constructor() {
    // Private constructor, singleton
    this.init();
  }

  static getInstance() {
    if (!EnvironmentConfigService.instance) {
      EnvironmentConfigService.instance = new EnvironmentConfigService();
    }
    return EnvironmentConfigService.instance;
  }

  private init() {
    if (!window) {
      return;
    }

    const win = window as any;
    const DeckGo = win.DeckGo;

    this.m = new Map<keyof EnvironmentConfig, any>(Object.entries(DeckGo.config) as any);
  }

  get(key: keyof EnvironmentConfig, fallback?: any): any {
    const value = this.m.get(key);
    return value !== undefined ? value : fallback;
  }
}
