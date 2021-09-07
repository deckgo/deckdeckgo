import firebase from 'firebase/app';

import {del, get, set} from 'idb-keyval';

import authStore from '../../../stores/auth.store';
import syncStore from '../../../stores/sync.store';
import offlineStore from '../../../stores/offline.store';

import {Deck, DeckAttributes} from '../../../models/data/deck';
import {Slide, SlideAttributes} from '../../../models/data/slide';

import {SlotType} from '../../../types/editor/slot-type';
import {SyncData, SyncDataDeck, SyncDataSlide} from '../../../types/editor/sync';

import {FirestoreUtils} from '../../../utils/editor/firestore.utils';
import {firebase as firebaseEnabled} from '../../../utils/core/environment.utils';
import {deckSelector} from '../../../utils/editor/deck.utils';

import {SlideFirebaseProvider} from '../../../providers/data/slide/slide.firebase.provider';
import {DeckFirebaseProvider} from '../../../providers/data/deck/deck.firebase.provider';
import {StorageFirebaseService} from '../../storage/storage.firebase.service';
import {SyncService} from './sync.service';

export class SyncFirebaseService extends SyncService {
  private slideFirebaseService: SlideFirebaseProvider;
  private deckFirebaseProvider: DeckFirebaseProvider;

  private storageFirebaseService: StorageFirebaseService;

  constructor() {
    super();

    this.deckFirebaseProvider = DeckFirebaseProvider.getInstance();
    this.slideFirebaseService = SlideFirebaseProvider.getInstance();
    this.storageFirebaseService = StorageFirebaseService.getInstance();
  }

  // @Override
  async upload(syncData: SyncData | undefined) {
    try {
      if (!syncData) {
        return;
      }

      if (!authStore.state.loggedIn || !offlineStore.state.online) {
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

      const {updateDecks, updateSlides, deleteSlides} = syncData;

      // First decks because it contains information for the permission and the list of slides
      await this.uploadDecks(updateDecks);

      await this.uploadSlides(updateSlides);

      await this.deleteSlides(deleteSlides);

      // TODO: handle delete decks here?

      await this.clean(syncData);
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

        const src: string = (slideElement as HTMLDeckgoSlideChartElement).src;

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
        const storageFile: StorageFile | undefined = await this.storageFirebaseService.uploadFile(data, 'data', 10485760);

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
        const storageFile: StorageFile | undefined = await this.storageFirebaseService.uploadFile(data, 'images', 10485760);

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

        const persistedDeck: Deck = await this.deckFirebaseProvider.update(deck);

        resolve(persistedDeck);
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadDeckBackgroundAssets(deckId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      const backgroundElement: HTMLElement = document.querySelector(`${deckSelector} > *[slot="background"]`);

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
        const storageFile: StorageFile | undefined = await this.storageFirebaseService.uploadFile(data, 'images', 10485760);

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
