import firebase from 'firebase/app';

import {del, get, set, update} from 'idb-keyval';

import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';
import offlineStore from '../../../stores/offline.store';

import {Deck, DeckAttributes} from '../../../models/data/deck';
import {Slide, SlideAttributes} from '../../../models/data/slide';

import {SlotType} from '../../../types/editor/slot-type';
import {SyncData, SyncDataDeck, SyncDataSlide, SyncPending, SyncPendingDeck} from '../../../types/editor/sync';

import {FirestoreUtils} from '../../../utils/editor/firestore.utils';
import {firebaseEnabled} from '../../../utils/core/environment.utils';
import {deckSelector} from '../../../utils/editor/deck.utils';

import {SlideOnlineService} from '../../data/slide/slide.online.service';
import {DeckOnlineService} from '../../data/deck/deck.online.service';

import {StorageOnlineService} from '../../storage/storage.online.service';

export class SyncService {
  private static instance: SyncService;

  private slideOnlineService: SlideOnlineService;
  private deckOnlineService: DeckOnlineService;
  private storageOnlineService: StorageOnlineService;

  private constructor() {
    this.deckOnlineService = DeckOnlineService.getInstance();
    this.slideOnlineService = SlideOnlineService.getInstance();
    this.storageOnlineService = StorageOnlineService.getInstance();
  }

  static getInstance(): SyncService {
    if (!SyncService.instance) {
      SyncService.instance = new SyncService();
    }
    return SyncService.instance;
  }

  async upload(syncData: SyncData | undefined) {
    try {
      if (!syncData) {
        return;
      }

      if (!authStore.state.loggedIn || !offlineStore.state.online) {
        return;
      }

      if (syncStore.state.sync !== 'pending') {
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

      await this.initSyncState();
    } catch (err) {
      syncStore.state.sync = 'error';
      console.error(err);
    }
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
      const slideElement: HTMLElement = document.querySelector(`${deckSelector} > *[slide_id="${slideId}"]`);

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
        const storageFile: StorageFile | undefined = await this.storageOnlineService.uploadFile(data, 'data', 10485760);

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
        (slideElement as HTMLDeckgoSlideChartElement).src = storageFile.downloadUrl;

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
        const storageFile: StorageFile | undefined = await this.storageOnlineService.uploadFile(data, 'images', 10485760);

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

        slide.data.attributes = (await FirestoreUtils.prepareAttributes(slide.data.attributes)) as SlideAttributes;

        if (slide.data.content === null) {
          // @ts-ignore
          slide.data.content = firebase.firestore.FieldValue.delete();
        }

        await this.slideOnlineService.update(deckId, slide);

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

    const promises: Promise<void>[] = data.map(({deckId, slideId}: SyncDataSlide) => this.slideOnlineService.delete(deckId, slideId));
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

        deck.data.attributes = (await FirestoreUtils.prepareAttributes(deck.data.attributes)) as DeckAttributes;

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

        const persistedDeck: Deck = await this.deckOnlineService.update(deck);

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
        const storageFile: StorageFile | undefined = await this.storageOnlineService.uploadFile(data, 'images', 10485760);

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

  private async cleanPending(syncedAt: Date) {
    const data: SyncPending | undefined = await get<SyncPending>('deckdeckgo_pending_sync');

    if (!data) {
      return undefined;
    }

    const filter = (arr: SyncPendingDeck[]) => arr.filter(({queuedAt}: SyncPendingDeck) => queuedAt.getTime() > syncedAt.getTime());

    await update<SyncPending>(
      'deckdeckgo_pending_sync',
      (data: SyncPending) =>
        ({
          updateDecks: filter(data.updateDecks),
          deleteDecks: filter(data.deleteDecks),
          updateSlides: filter(data.updateSlides),
          deleteSlides: filter(data.deleteSlides)
        } as SyncPending)
    );
  }

  async initSyncState() {
    const data: SyncPending | undefined = await get<SyncPending>('deckdeckgo_pending_sync');

    if (!data) {
      syncStore.state.sync = 'idle';
      return;
    }

    const {updateDecks, deleteDecks, deleteSlides, updateSlides} = data;

    if (updateDecks.length === 0 && deleteDecks.length === 0 && deleteSlides.length === 0 && updateSlides.length === 0) {
      syncStore.state.sync = 'idle';
      return;
    }

    syncStore.state.sync = 'pending';
  }
}
