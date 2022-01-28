import {EnvironmentConfig} from '../../config/environment-config';

export class EnvironmentConfigService {
  private static instance: EnvironmentConfigService;

  private m: Map<keyof EnvironmentConfig, any>;

  private constructor() {
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

  get<T>(key: keyof EnvironmentConfig): T | undefined {
    return this.m.get(key);
  }
}
