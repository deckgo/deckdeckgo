import {EnvironmentDeckDeckGoConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../environment/environment-config.service';

export class AssetsService {
  private static instance: AssetsService;

  private assetsList: Assets | undefined = undefined;

  private constructor() {}

  static getInstance() {
    if (!AssetsService.instance) {
      AssetsService.instance = new AssetsService();
    }
    return AssetsService.instance;
  }

  async assets(): Promise<Assets> {
    if (this.assetsList === undefined) {
      this.assetsList = await this.init();
    }

    return this.assetsList;
  }

  private async init(): Promise<Assets> {
    try {
      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

      const res: Response = await fetch(`${config.globalAssetsUrl}/assets.json`);

      if (!res) {
        return undefined;
      }

      const assets: Assets = await res.json();

      return assets;
    } catch (err) {
      return undefined;
    }
  }
}
