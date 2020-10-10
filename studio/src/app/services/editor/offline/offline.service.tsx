import * as firebase from 'firebase/app';

import {del, get, set} from 'idb-keyval';

import deckStore from '../../../stores/deck.store';
import offlineStore from '../../../stores/offline.store';

import {Deck, DeckAttributes} from '../../../models/data/deck';
import {Slide, SlideAttributes} from '../../../models/data/slide';

import {SlotType} from '../../../utils/editor/slot-type';

import {OfflineUtils} from '../../../utils/editor/offline.utils';
import {FirestoreUtils} from '../../../utils/editor/firestore.utils';
import {ServiceWorkerUtils} from '../../../utils/core/service-worker-utils';

import {SlideOnlineService} from '../../data/slide/slide.online.service';
import {DeckOnlineService} from '../../data/deck/deck.online.service';
import {AssetsService} from '../../core/assets/assets.service';

import {EnvironmentDeckDeckGoConfig} from '../../core/environment/environment-config';
import {EnvironmentConfigService} from '../../core/environment/environment-config.service';
import {StorageOnlineService} from '../../storage/storage.online.service';
import {FontsService} from '../fonts/fonts.service';

export class OfflineService {
  private static instance: OfflineService;

  private slideOnlineService: SlideOnlineService;
  private deckOnlineService: DeckOnlineService;
  private storageOnlineService: StorageOnlineService;

  private assetsService: AssetsService;
  private fontsService: FontsService;

  private constructor() {
    this.deckOnlineService = DeckOnlineService.getInstance();
    this.slideOnlineService = SlideOnlineService.getInstance();
    this.storageOnlineService = StorageOnlineService.getInstance();

    this.assetsService = AssetsService.getInstance();
    this.fontsService = FontsService.getInstance();
  }

  static getInstance(): OfflineService {
    if (!OfflineService.instance) {
      OfflineService.instance = new OfflineService();
    }
    return OfflineService.instance;
  }

  async status(): Promise<OfflineDeck> {
    if (offlineStore.state.offline === undefined) {
      const saved: OfflineDeck = await get('deckdeckgo_offline');

      offlineStore.state.offline = saved ? {...saved} : undefined;

      return saved;
    }

    return offlineStore.state.offline;
  }

  async init() {
    await this.status();
  }

  private progress(progress: number) {
    offlineStore.state.progress += progress;
  }

  private progressStart() {
    offlineStore.state.progress = 0;
  }

  private progressComplete() {
    offlineStore.state.progress = 1;
  }

  save(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        this.progressStart();

        const promises: Promise<void>[] = [this.saveDeck(), this.lazyLoadAllContent()];

        await Promise.all(promises);

        await this.cacheServiceWorker();

        await this.toggleOffline();

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  upload(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        this.progressStart();

        await this.uploadData();

        await del('deckdeckgo_offline');

        this.progressComplete();

        offlineStore.reset();

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private toggleOffline(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        try {
          if (!deckStore.state.deck || !deckStore.state.deck.id || !deckStore.state.deck.data) {
            reject('No deck found');
            return;
          }

          const offline: OfflineDeck = {
            id: deckStore.state.deck.id,
            name: deckStore.state.deck.data.name,
          };

          await set('deckdeckgo_offline', offline);

          offlineStore.state.offline = {...offline};

          this.progressComplete();

          resolve();
        } catch (err) {
          reject(err);
        }
      } catch (err) {
        reject(err);
      }
    });
  }

  private cacheServiceWorker(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const deckElement: HTMLElement = document.querySelector('app-editor > ion-content > main > deckgo-deck');

        if (!deckElement) {
          reject('No deck found');
          return;
        }

        await this.cacheImages(deckElement);

        this.progress(0.2);

        await this.cacheAssets();

        this.progress(0.1);

        await this.fontsService.loadAllGoogleFonts();

        this.progress(0.1);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
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

    await ServiceWorkerUtils.cacheUrls('cors-images', list);
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

    await ServiceWorkerUtils.cacheUrls('images', list);
  }

  private async cacheAssets() {
    const assets: Assets | undefined = await this.assetsService.assets();

    if (assets === undefined) {
      return;
    }

    const promises: Promise<void>[] = [
      this.assetsDefinition(),
      this.assetsShapes(assets),
      this.assetsDeckDeckGo(assets),
      this.assetsNavigation(assets),
      this.assetCharts(assets),
    ];

    // We don't cache PrismJS definition file.
    // If we would do so, then the list of languages would be displayed but because we load on the fly, it would be in any case not possible offline to fetch the proper definition

    await Promise.all(promises);
  }

  private async assetsDefinition(): Promise<void> {
    const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    const assetsFileUrl: string[] = [`${config.globalAssetsUrl}/assets.json`];

    await ServiceWorkerUtils.cacheUrls('assets', assetsFileUrl);
  }

  private async assetsShapes(assets: Assets): Promise<void> {
    const deckGoUrls: string[] = [
      ...this.assetsShapesList(assets, 'shapes'),
      ...this.assetsShapesList(assets, 'arrows'),
      ...this.assetsShapesList(assets, 'status'),
      ...this.assetsShapesList(assets, 'computers'),
      ...this.assetsShapesList(assets, 'dateTime'),
      ...this.assetsShapesList(assets, 'files'),
      ...this.assetsShapesList(assets, 'finance'),
    ];

    await ServiceWorkerUtils.cacheUrls('images', deckGoUrls);
  }

  private async assetsDeckDeckGo(assets: Assets): Promise<void> {
    if (assets.deckdeckgo) {
      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');
      const deckGoUrls: string[] = [`${config.globalAssetsUrl}${assets.deckdeckgo.logo}`];

      await ServiceWorkerUtils.cacheUrls('images', deckGoUrls);
    }
  }

  private async assetsNavigation(assets: Assets) {
    if (assets.navigation && assets.navigation.length > 0) {
      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

      const deckGoUrls: string[] = assets.navigation.map((asset: ImgAsset) => {
        return `${config.globalAssetsUrl}${asset.src}`;
      });

      await ServiceWorkerUtils.cacheUrls('images', deckGoUrls);
    }
  }

  private assetsShapesList(assets: Assets, group: string): string[] {
    if (assets.shapes && assets.shapes[group] && assets.shapes[group].length > 0) {
      const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

      return assets.shapes[group].map((asset: ImgAsset) => {
        return `${config.globalAssetsUrl}${asset.src}`;
      });
    } else {
      return [];
    }
  }

  private async assetCharts(assets: Assets): Promise<void> {
    if (assets.chart) {
      const corsGitHubUrls: string[] = Object.keys(assets.chart).map((key: string) => {
        return assets.chart[key];
      });

      await ServiceWorkerUtils.cacheUrls('data-content', corsGitHubUrls);
    }
  }

  private lazyLoadAllContent(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const deck = document.querySelector('app-editor > ion-content > main > deckgo-deck');

        if (!deck) {
          reject('Deck not found');
          return;
        }

        await (deck as any).lazyLoadAllContent();

        this.progress(0.1);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private saveDeck(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (!deckStore.state.deck || !deckStore.state.deck.id || !deckStore.state.deck.data) {
          reject('No deck found');
          return;
        }

        await this.saveSlides(deckStore.state.deck);

        if (deckStore.state.deck.data.background && FirestoreUtils.shouldAttributeBeCleaned(deckStore.state.deck.data.background)) {
          deckStore.state.deck.data.background = null;
        }

        await set(`/decks/${deckStore.state.deck.id}`, deckStore.state.deck);

        this.progress(0.5);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private saveSlides(deck: Deck): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      if (!deck.data.slides || deck.data.slides.length <= 0) {
        resolve();
        return;
      }

      try {
        const promises: Promise<Slide>[] = [];

        for (let i: number = 0; i < deck.data.slides.length; i++) {
          const slideId: string = deck.data.slides[i];

          promises.push(this.saveSlide(deck, slideId));
        }

        if (!promises || promises.length <= 0) {
          resolve();
          return;
        }

        await Promise.all(promises);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private saveSlide(deck: Deck, slideId: string): Promise<Slide> {
    return new Promise<Slide>(async (resolve, reject) => {
      const slide: Slide = await this.slideOnlineService.get(deck.id, slideId);

      if (!slide || !slide.data) {
        reject('Missing slide for publishing');
        return;
      }

      await set(`/decks/${deck.id}/slides/${slideId}`, slide);

      this.progress(deck.data?.slides?.length > 0 ? 0.4 / deck.data.slides.length : 0);

      resolve(slide);
    });
  }

  private uploadData(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (!deckStore.state.deck || !deckStore.state.deck.id || !deckStore.state.deck.data) {
          reject('No deck found');
          return;
        }

        await this.uploadSlides(deckStore.state.deck);

        await this.deleteSlides(deckStore.state.deck);

        const persistedDeck: Deck = await this.uploadDeck(deckStore.state.deck);

        deckStore.state.deck = {...persistedDeck};

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadSlides(deck: Deck): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      if (!deck.data.slides || deck.data.slides.length <= 0) {
        resolve();
        return;
      }

      try {
        const promises: Promise<void>[] = [];

        for (let i: number = 0; i < deck.data.slides.length; i++) {
          const slideId: string = deck.data.slides[i];

          promises.push(this.uploadSlide(deck, slideId));
        }

        if (!promises || promises.length <= 0) {
          resolve();
          return;
        }

        await Promise.all(promises);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async uploadSlide(deck: Deck, slideId: string): Promise<void> {
    await this.uploadSlideLocalUserAssets(deck, slideId);
    await this.uploadSlideData(deck, slideId);

    this.progress(deck.data?.slides?.length > 0 ? 0.7 / deck.data.slides.length : 0);
  }

  private uploadSlideLocalUserAssets(deck: Deck, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      const slideElement: HTMLElement = document.querySelector(`app-editor > ion-content > main > deckgo-deck > *[slide_id="${slideId}"]`);

      if (!slideElement) {
        reject('No slide found');
        return;
      }

      try {
        await this.uploadSlideLocalCharts(slideElement, deck, slideId);
        await this.uploadSlideLocalImages(slideElement, deck, slideId);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadSlideLocalCharts(slideElement: HTMLElement, deck: Deck, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        if (slideElement.tagName && slideElement.tagName.toUpperCase() !== 'deckgo-slide-chart'.toUpperCase()) {
          resolve();
          return;
        }

        const src: string = (slideElement as any).src;

        if (!src || src === undefined || src === '') {
          resolve();
          return;
        }

        const data: File = await get(src);

        if (!data) {
          // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
          // Better than blocking the all process and reaching an intermediate state.
          resolve();
          return;
        }

        // 1. We upload the file to the storage cloud
        const storageFile: StorageFile = await this.storageOnlineService.uploadFile(data, 'data', 10485760);

        if (!storageFile) {
          reject(`Chart ${src} upload has failed.`);
          return;
        }

        // 2. We update the indexedDB stored slide with the new downloadUrl. This stored slide will be later updated back to the database.
        const slide: Slide = await get(`/decks/${deck.id}/slides/${slideId}`);

        if (!slide) {
          reject('Slide not found and that is really weird here.');
          return;
        }

        slide.data.attributes.src = storageFile.downloadUrl;

        await set(`/decks/${deck.id}/slides/${slideId}`, slide);

        // 3. We update the DOM
        (slideElement as any).src = storageFile.downloadUrl;

        // 4. All good, we don't need the image in the indexedDB anymore
        await del(src);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadSlideLocalImages(slideElement: HTMLElement, deck: Deck, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const imgs: NodeListOf<HTMLDeckgoLazyImgElement> = slideElement.querySelectorAll(SlotType.IMG);

        if (!imgs || imgs.length <= 0) {
          resolve();
          return;
        }

        // Filter online images (http...) and deck background (which are cloned from the deck to the slides)
        const list: HTMLDeckgoLazyImgElement[] = Array.from(imgs).filter((img: HTMLDeckgoLazyImgElement) => {
          return (
            img.imgSrc !== undefined &&
            img.imgSrc !== '' &&
            img.imgSrc.indexOf('http') === -1 &&
            !(img.parentElement && img.parentElement.getAttribute('slot') === 'background' && !slideElement.hasAttribute('custom-background'))
          );
        });

        if (!list || list.length <= 0) {
          resolve();
          return;
        }

        const promises: Promise<void>[] = list.map((img: HTMLDeckgoLazyImgElement) => {
          return this.uploadSlideLocalImage(img, deck, slideId);
        });

        await Promise.all(promises);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadSlideLocalImage(img: HTMLDeckgoLazyImgElement, deck: Deck, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const data: File = await get(img.imgSrc);

        if (!data) {
          // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
          // Better than blocking the all process and reaching an intermediate state.
          resolve();
          return;
        }

        // 1. We upload the file to the storage cloud
        const storageFile: StorageFile = await this.storageOnlineService.uploadFile(data, 'images', 10485760);

        if (!storageFile) {
          reject(`Image ${img.imgSrc} upload has failed.`);
          return;
        }

        // 2. We update the indexedDB stored slide with the new downloadUrl. This stored slide will be later uploaded back to the database.
        const slide: Slide = await get(`/decks/${deck.id}/slides/${slideId}`);

        if (!slide) {
          reject('Slide not found and that is really weird here.');
          return;
        }

        slide.data.content = slide.data.content.replace(`img-src="${img.imgSrc}"`, `img-src="${storageFile.downloadUrl}"`);
        slide.data.content = slide.data.content.replace(`img-alt="${img.imgSrc}"`, `img-alt="${storageFile.downloadUrl}"`);

        await set(`/decks/${deck.id}/slides/${slideId}`, slide);

        // 3. We update the DOM
        img.imgSrc = storageFile.downloadUrl;
        img.imgAlt = storageFile.downloadUrl;

        // 4. All good, we don't need the image in the indexedDB anymore
        await del(img.imgSrc);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadSlideData(deck: Deck, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const slide: Slide = await get(`/decks/${deck.id}/slides/${slideId}`);

        if (!slide || !slide.data) {
          // If upload process end up in error in a previous try, some slides might have already been uploaded correctly and remove from the local db
          resolve();
          return;
        }

        slide.data.attributes = (await OfflineUtils.prepareAttributes(slide.data.attributes)) as SlideAttributes;

        if (slide.data.content === null) {
          // @ts-ignore
          slide.data.content = firebase.firestore.FieldValue.delete();
        }

        await this.slideOnlineService.update(deck.id, slide);

        await del(`/decks/${deck.id}/slides/${slideId}`);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async uploadDeck(deck: Deck): Promise<Deck> {
    await this.uploadDeckBackgroundAssets(deck);

    const persistedDeck: Deck = await this.uploadDeckData(deck);

    this.progress(0.1);

    return persistedDeck;
  }

  private uploadDeckData(deck: Deck): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        if (!deck || !deck.data) {
          reject('Missing deck for upload');
          return;
        }

        deck.data.attributes = (await OfflineUtils.prepareAttributes(deck.data.attributes)) as DeckAttributes;

        if (deck.data.background === null) {
          // @ts-ignore
          deck.data.background = firebase.firestore.FieldValue.delete();
        }

        const persistedDeck: Deck = await this.deckOnlineService.update(deck);

        await del(`/decks/${deck.id}`);

        resolve(persistedDeck);
      } catch (err) {
        reject(err);
      }
    });
  }

  private deleteSlides(deck: Deck): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      if (!deck) {
        resolve();
        return;
      }

      try {
        const slidesToDelete: string[] = await get('deckdeckgo_slides_delete');

        if (!slidesToDelete || slidesToDelete.length <= 0) {
          resolve();
          return;
        }

        const promises: Promise<void>[] = slidesToDelete.map((slideId: string) => {
          return this.deleteSlide(deck, slideId, slidesToDelete.length);
        });

        if (!promises || promises.length <= 0) {
          resolve();
          return;
        }

        await Promise.all(promises);

        await del('deckdeckgo_slides_delete');

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private deleteSlide(deck: Deck, slideId: string, slidesToDeleteLength: number): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        await this.slideOnlineService.delete(deck.id, slideId);

        this.progress(0.3 / slidesToDeleteLength);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadDeckBackgroundAssets(deck: Deck): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      const backgroundElement: HTMLElement = document.querySelector(`app-editor > ion-content > main > deckgo-deck > *[slot="background"]`);

      if (!backgroundElement) {
        resolve();
        return;
      }

      try {
        const img: HTMLDeckgoLazyImgElement = backgroundElement.querySelector(SlotType.IMG);

        if (!img) {
          resolve();
          return;
        }

        await this.uploadDeckLocalImage(img, deck.id);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadDeckLocalImage(img: HTMLDeckgoLazyImgElement, deckId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const data: File = await get(img.imgSrc);

        if (!data) {
          // We didn't the corresponding image. Instead of crashing an error we go through, user will notice that nothing is displayed.
          // Better than blocking the all process and reaching an intermediate state.
          resolve();
          return;
        }

        // 1. We upload the file to the storage cloud
        const storageFile: StorageFile = await this.storageOnlineService.uploadFile(data, 'images', 10485760);

        if (!storageFile) {
          reject(`Image ${img.imgSrc} upload has failed.`);
          return;
        }

        // 2. We update the indexedDB stored deck with the new downloadUrl. This stored deck will be later updated back to the database.
        const deck: Deck = await get(`/decks/${deckId}`);

        if (!deck) {
          reject('Deck not found and that is really weird here.');
          return;
        }

        deck.data.background = deck.data.background.replace(`img-src="${img.imgSrc}"`, `img-src="${storageFile.downloadUrl}"`);
        deck.data.background = deck.data.background.replace(`img-alt="${img.imgSrc}"`, `img-alt="${storageFile.downloadUrl}"`);

        await set(`/decks/${deck.id}`, deck);

        // 3. We update the DOM
        img.imgSrc = storageFile.downloadUrl;
        img.imgAlt = storageFile.downloadUrl;

        // 4. All good, we don't need the image in the indexedDB anymore
        await del(img.imgSrc);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }
}
