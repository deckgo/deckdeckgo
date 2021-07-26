import firebase from 'firebase/app';

import {del, get, set} from 'idb-keyval';

import deckStore from '../../../stores/deck.store';
import offlineStore from '../../../stores/offline.store';
import assetsStore from '../../../stores/assets.store';
import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';

import {Deck, DeckAttributes} from '../../../models/data/deck';
import {Slide, SlideAttributes} from '../../../models/data/slide';

import {SlotType} from '../../../types/editor/slot-type';
import {SyncData, SyncDataDeck, SyncDataSlide} from '../../../types/editor/sync';

import {OfflineUtils} from '../../../utils/editor/offline.utils';
import {FirestoreUtils} from '../../../utils/editor/firestore.utils';
import {ServiceWorkerUtils} from '../../../utils/core/service-worker.utils';
import {firebase as firebaseEnabled} from '../../../utils/core/environment.utils';

import {SlideFirebaseService} from '../../data/slide/slide.firebase.service';
import {DeckFirebaseService} from '../../data/deck/deck.firebase.service';

import {EnvironmentDeckDeckGoConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../environment/environment-config.service';
import {StorageService} from '../../storage/storage.service';
import {FontsService} from '../fonts/fonts.service';
import {SyncService} from './sync.service';
import {StorageFactoryService} from '../../storage/storage.factory.service';

export class SyncFirebaseService extends SyncService {
  private slideFirebaseService: SlideFirebaseService;
  private deckFirebaseService: DeckFirebaseService;

  private storageService: StorageService;

  private fontsService: FontsService;

  constructor() {
    super();

    this.deckFirebaseService = DeckFirebaseService.getInstance();
    this.slideFirebaseService = SlideFirebaseService.getInstance();
    this.storageService = StorageFactoryService.getInstance();
    this.fontsService = FontsService.getInstance();
  }

  // TODO: This method will probably be removed
  save(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
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

  // @Override
  async upload(syncData: SyncData | undefined) {
    try {
      if (!syncData) {
        return;
      }

      if (!authStore.state.loggedIn || !navigator.onLine) {
        return;
      }

      if (!this.isSyncPending()) {
        return;
      }

      if (!firebaseEnabled()) {
        return;
      }

      syncStore.state.sync = 'in_progress';

      // TODO: when we will solve the storage question, we can leverage the data provided as parameter instead of querying idb here again

      const {syncedAt, updateDecks, updateSlides, deleteSlides} = syncData;

      // First decks because it contains information for the permission and the list of slides
      await this.uploadDecks(updateDecks);

      await this.uploadSlides(updateSlides);

      await this.deleteSlides(deleteSlides);

      // TODO: handle delete decks here?

      await this.cleanPending(syncedAt);

      await this.updateSyncState();
    } catch (err) {
      syncStore.state.sync = 'error';
      console.error(err);
    }
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
            name: deckStore.state.deck.data.name
          };

          await set('deckdeckgo_offline', offline);

          offlineStore.state.offline = {...offline};

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
        const deckElement: HTMLElement = document.querySelector('app-editor > ion-content div.deck > main > deckgo-deck');

        if (!deckElement) {
          resolve();
          return;
        }

        await this.cacheImages(deckElement);

        await this.cacheAssets();

        await this.fontsService.loadAllGoogleFonts();

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

  private lazyLoadAllContent(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const deck: HTMLDeckgoDeckElement = document.querySelector('app-editor > ion-content div.deck > main > deckgo-deck');

        if (!deck) {
          resolve();
          return;
        }

        await deck.lazyLoadAllContent();

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

        if (deckStore.state.deck.data.header && FirestoreUtils.shouldAttributeBeCleaned(deckStore.state.deck.data.header)) {
          deckStore.state.deck.data.header = null;
        }

        if (deckStore.state.deck.data.footer && FirestoreUtils.shouldAttributeBeCleaned(deckStore.state.deck.data.footer)) {
          deckStore.state.deck.data.footer = null;
        }

        await set(`/decks/${deckStore.state.deck.id}`, deckStore.state.deck);

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
      const slide: Slide = await this.slideFirebaseService.get(deck.id, slideId);

      if (!slide || !slide.data) {
        reject('Missing slide for publishing');
        return;
      }

      await set(`/decks/${deck.id}/slides/${slideId}`, slide);

      resolve(slide);
    });
  }

  private uploadSlides(data: SyncDataSlide[] | undefined): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      if (!data || data.length <= 0) {
        resolve();
        return;
      }

      try {
        const promises: Promise<void>[] = data.map(({deckId, slideId}: SyncDataSlide) => this.uploadSlide(deckId, slideId));

        await Promise.all(promises);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async uploadSlide(deckId: string, slideId: string): Promise<void> {
    await this.uploadSlideLocalUserAssets(deckId, slideId);
    await this.uploadSlideData(deckId, slideId);
  }

  private uploadSlideLocalUserAssets(deckId: string, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      const slideElement: HTMLElement = document.querySelector(`app-editor > ion-content div.deck > main > deckgo-deck > *[slide_id="${slideId}"]`);

      if (!slideElement) {
        resolve();
        return;
      }

      try {
        await this.uploadSlideLocalCharts(slideElement, deckId, slideId);
        await this.uploadSlideLocalImages(slideElement, deckId, slideId);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadSlideLocalCharts(slideElement: HTMLElement, deckId: string, slideId: string): Promise<void> {
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
        const storageFile: StorageFile | undefined = await this.storageService.uploadFile(data, 'data', 10485760);

        if (!storageFile) {
          reject(`Chart ${src} upload has failed.`);
          return;
        }

        // 2. We update the indexedDB stored slide with the new downloadUrl. This stored slide will be later updated back to the database.
        const slide: Slide = await get(`/decks/${deckId}/slides/${slideId}`);

        if (!slide) {
          reject('Slide not found and that is really weird here.');
          return;
        }

        slide.data.attributes.src = storageFile.downloadUrl;

        await set(`/decks/${deckId}/slides/${slideId}`, slide);

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

  private uploadSlideLocalImages(slideElement: HTMLElement, deckId: string, slideId: string): Promise<void> {
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
          return this.uploadSlideLocalImage(img, deckId, slideId);
        });

        await Promise.all(promises);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadSlideLocalImage(img: HTMLDeckgoLazyImgElement, deckId: string, slideId: string): Promise<void> {
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
        const storageFile: StorageFile | undefined = await this.storageService.uploadFile(data, 'images', 10485760);

        if (!storageFile) {
          reject(`Image ${img.imgSrc} upload has failed.`);
          return;
        }

        // 2. We update the indexedDB stored slide with the new downloadUrl. This stored slide will be later uploaded back to the database.
        const slide: Slide = await get(`/decks/${deckId}/slides/${slideId}`);

        if (!slide) {
          reject('Slide not found and that is really weird here.');
          return;
        }

        slide.data.content = slide.data.content.replace(`img-src="${img.imgSrc}"`, `img-src="${storageFile.downloadUrl}"`);
        slide.data.content = slide.data.content.replace(`img-alt="${img.imgSrc}"`, `img-alt="${storageFile.downloadUrl}"`);

        await set(`/decks/${deckId}/slides/${slideId}`, slide);

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

  private uploadSlideData(deckId: string, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const slide: Slide = await get(`/decks/${deckId}/slides/${slideId}`);

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

        await this.slideFirebaseService.update(deckId, slide);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private async deleteSlides(data: SyncDataSlide[] | undefined): Promise<void> {
    if (!data || data.length <= 0) {
      return;
    }

    const promises: Promise<void>[] = data.map(({deckId, slideId}: SyncDataSlide) => this.slideFirebaseService.delete(deckId, slideId));
    await Promise.all(promises);
  }

  private async uploadDecks(data: SyncDataDeck[] | undefined): Promise<void> {
    if (!data || data.length <= 0) {
      return;
    }

    const promises: Promise<void>[] = data.map((deck: SyncDataDeck) => this.uploadDeck(deck));
    await Promise.all(promises);
  }

  private async uploadDeck({deckId}: SyncDataDeck): Promise<void> {
    await this.uploadDeckBackgroundAssets(deckId);

    const deck: Deck = await get(`/decks/${deckId}`);

    if (!deck) {
      return;
    }

    await this.uploadDeckData({
      id: deckId,
      data: {
        ...deck.data,
        owner_id: authStore.state.authUser.uid
      }
    });
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

        if (deck.data.header === null) {
          // @ts-ignore
          deck.data.header = firebase.firestore.FieldValue.delete();
        }

        if (deck.data.footer === null) {
          // @ts-ignore
          deck.data.footer = firebase.firestore.FieldValue.delete();
        }

        const persistedDeck: Deck = await this.deckFirebaseService.update(deck);

        resolve(persistedDeck);
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadDeckBackgroundAssets(deckId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      const backgroundElement: HTMLElement = document.querySelector(`app-editor > ion-content div.deck > main > deckgo-deck > *[slot="background"]`);

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

        await this.uploadDeckLocalImage(img, deckId);

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
        const storageFile: StorageFile | undefined = await this.storageService.uploadFile(data, 'images', 10485760);

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
