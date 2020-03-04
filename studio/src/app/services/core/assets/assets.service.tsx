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
      // TODO
      // Replace with prod link

      // const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      // `${config.globalAssetsUrl}/assets.json`

      const res: Response = await fetch(`http://localhost:3333/assets/assets.json`);

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
