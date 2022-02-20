import {deckSelector} from '@deckdeckgo/editor';
import {offlineStore, SlotType} from '@deckdeckgo/studio';
import {EnvironmentDeckDeckGoConfig} from '../../../config/environment-config';
import assetsStore from '../../../stores/assets.store';
import {ServiceWorkerUtils} from '../../../utils/core/service-worker.utils';
import {EnvironmentConfigService} from '../../environment/environment-config.service';
import {FontsService} from '../fonts/fonts.service';

export class SwService {
  private static instance: SwService;

  private fontsService: FontsService;

  private constructor() {
    this.fontsService = FontsService.getInstance();
  }

  static getInstance(): SwService {
    if (!SwService.instance) {
      SwService.instance = new SwService();
    }
    return SwService.instance;
  }

  async cacheServiceWorker() {
    if (!offlineStore.default.state.online) {
      return;
    }

    try {
      const deckElement: HTMLElement = document.querySelector(deckSelector);

      if (!deckElement) {
        return;
      }

      await this.cacheImages(deckElement);

      await this.cacheAssets();

      await this.fontsService.loadAllGoogleFonts();
    } catch (err) {
      // We can live with the error
      console.error(err);
    }
  }

  private async cacheImages(deckElement: HTMLElement) {
    const promises: Promise<void>[] = [this.cacheCorsImages(deckElement), this.cacheDeckGoImages(deckElement)];

    await Promise.all(promises);
  }

  private async cacheCorsImages(deckElement: HTMLElement): Promise<void> {
    const imgs: NodeListOf<HTMLDeckgoLazyImgElement> = deckElement.querySelectorAll(SlotType.IMG);

    if (!imgs) {
      return;
    }

    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    const list: string[] = Array.from(imgs)
      .filter((img: HTMLDeckgoLazyImgElement) => {
        return (
          (img.imgSrc !== undefined && img.imgSrc !== '' && img.imgSrc.indexOf(config.globalAssetsUrl) === -1) ||
          (img.svgSrc !== undefined && img.svgSrc !== '' && img.svgSrc.indexOf(config.globalAssetsUrl) === -1)
        );
      })
      .map((img) => {
        return img.imgSrc || img.svgSrc;
      });

    if (!list || list.length <= 0) {
      return;
    }

    await ServiceWorkerUtils.cacheUrls('cors-images', [...new Set(list)]);
  }

  private async cacheDeckGoImages(deckElement: HTMLElement): Promise<void> {
    const imgs: NodeListOf<HTMLDeckgoLazyImgElement> = deckElement.querySelectorAll(SlotType.IMG);

    if (!imgs) {
      return;
    }

    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    const list = Array.from(imgs)
      .filter((img: HTMLDeckgoLazyImgElement) => {
        return (
          (img.imgSrc !== undefined && img.imgSrc !== '' && img.imgSrc.indexOf(config.globalAssetsUrl) > -1) ||
          (img.svgSrc !== undefined && img.svgSrc !== '' && img.svgSrc.indexOf(config.globalAssetsUrl) > -1)
        );
      })
      .map((img) => {
        return img.imgSrc || img.svgSrc;
      });

    if (!list || list.length <= 0) {
      return;
    }

    await ServiceWorkerUtils.cacheUrls('images', [...new Set(list)]);
  }

  private async cacheAssets() {
    const promises: Promise<void>[] = [this.assetsShapes(), this.assetsDeckDeckGo(), this.assetsNavigation(), this.assetCharts()];

    // We don't cache PrismJS definition file.
    // If we would do so, then the list of languages would be displayed but because we load on the fly, it would be in any case not possible offline to fetch the proper definition

    await Promise.all(promises);
  }

  private async assetsShapes(): Promise<void> {
    const deckGoUrls: string[] = [
      ...this.assetsShapesList('shapes'),
      ...this.assetsShapesList('arrows'),
      ...this.assetsShapesList('status'),
      ...this.assetsShapesList('computers'),
      ...this.assetsShapesList('dateTime'),
      ...this.assetsShapesList('files'),
      ...this.assetsShapesList('finance')
    ];

    await ServiceWorkerUtils.cacheUrls('images', deckGoUrls);
  }

  private async assetsDeckDeckGo(): Promise<void> {
    if (assetsStore.state.deckdeckgo) {
      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      const deckGoUrls: string[] = [`${config.globalAssetsUrl}${assetsStore.state.deckdeckgo.logo}`];

      await ServiceWorkerUtils.cacheUrls('images', deckGoUrls);
    }
  }

  private async assetsNavigation() {
    if (assetsStore.state.navigation && assetsStore.state.navigation.length > 0) {
      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

      const deckGoUrls: string[] = assetsStore.state.navigation.map((asset: ImgAsset) => {
        return `${config.globalAssetsUrl}${asset.src}`;
      });

      await ServiceWorkerUtils.cacheUrls('images', deckGoUrls);
    }
  }

  private assetsShapesList(group: string): string[] {
    if (assetsStore.state.shapes && assetsStore.state.shapes[group] && assetsStore.state.shapes[group].length > 0) {
      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

      return assetsStore.state.shapes[group].map((asset: ImgAsset) => {
        return `${config.globalAssetsUrl}${asset.src}`;
      });
    } else {
      return [];
    }
  }

  private async assetCharts(): Promise<void> {
    if (assetsStore.state.chart) {
      const corsGitHubUrls: string[] = Object.keys(assetsStore.state.chart).map((key: string) => {
        return assetsStore.state.chart[key];
      });

      await ServiceWorkerUtils.cacheUrls('data-content', corsGitHubUrls);
    }
  }
}
