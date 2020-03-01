import {firebase} from '@firebase/app';

import {del, get, set} from 'idb-keyval';

import {BehaviorSubject, Observable} from 'rxjs';
import {take} from 'rxjs/operators';

import {Deck} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';

import {SlotType} from '../../../utils/editor/slot-type';

import {OfflineUtils} from '../../../utils/editor/offline.utils';

import {DeckEditorService} from '../deck/deck-editor.service';
import {SlideOnlineService} from '../../data/slide/slide.online.service';
import {DeckOnlineService} from '../../data/deck/deck.online.service';

export class OfflineService {
  private static instance: OfflineService;

  private deckEditorService: DeckEditorService;
  private slideOnlineService: SlideOnlineService;
  private deckOnlineService: DeckOnlineService;

  private offlineSubject: BehaviorSubject<OfflineDeck | undefined> = new BehaviorSubject(undefined);

  private constructor() {
    this.deckEditorService = DeckEditorService.getInstance();
    this.deckOnlineService = DeckOnlineService.getInstance();
    this.slideOnlineService = SlideOnlineService.getInstance();
  }

  static getInstance(): OfflineService {
    if (!OfflineService.instance) {
      OfflineService.instance = new OfflineService();
    }
    return OfflineService.instance;
  }

  async status(): Promise<OfflineDeck> {
    return new Promise<OfflineDeck>((resolve) => {
      this.offlineSubject.pipe(take(1)).subscribe(async (offline: OfflineDeck | undefined) => {
        if (offline === undefined) {
          const saved: OfflineDeck = await get('deckdeckgo_offline');

          this.offlineSubject.next(saved);

          resolve(saved);
          return;
        }

        resolve(offline);
      });
    });
  }

  async init() {
    await this.status();
  }

  watchOffline(): Observable<OfflineDeck | undefined> {
    return this.offlineSubject.asObservable();
  }

  save(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const promises: Promise<void>[] = [this.saveDeck(), this.lazyLoadAllContent()];

        await Promise.all(promises);

        await this.addToSWCache();

        await this.toggleOffline();

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private toggleOffline(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      try {
        this.deckEditorService
          .watch()
          .pipe(take(1))
          .subscribe(async (deck: Deck) => {
            try {
              if (!deck || !deck.id || !deck.data) {
                reject('No deck found');
                return;
              }

              const offline: OfflineDeck = {
                id: deck.id,
                name: deck.data.name
              };

              await set('deckdeckgo_offline', offline);

              this.offlineSubject.next(offline);

              resolve();
            } catch (err) {
              reject(err);
            }
          });
      } catch (err) {
        reject(err);
      }
    });
  }

  upload(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        await this.uploadData();

        await del('deckdeckgo_offline');

        this.offlineSubject.next(undefined);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private addToSWCache(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      try {
        this.deckEditorService
          .watch()
          .pipe(take(1))
          .subscribe(async (deck: Deck) => {
            try {
              if (!deck || !deck.id || !deck.data) {
                reject('No deck found');
                return;
              }

              // TODO clean code
              // TODO add other caches / SlotType

              const imgs = document.querySelectorAll(SlotType.IMG);

              if (!imgs) {
                resolve();
                return;
              }

              const list = Array.from(imgs).map((img) => {
                return img.imgSrc;
              });

              const myCache = await window.caches.open('unsplash-images');
              await myCache.addAll(list);

              resolve();
            } catch (err) {
              reject(err);
            }
          });
      } catch (err) {
        reject(err);
      }
    });
  }

  private lazyLoadAllContent(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        // TODO don't find deck here
        const deck = document.querySelector('main > deckgo-deck');

        if (!deck) {
          reject('Deck not found');
          return;
        }

        await (deck as any).lazyLoadAllContent();

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private saveDeck(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      try {
        this.deckEditorService
          .watch()
          .pipe(take(1))
          .subscribe(async (deck: Deck) => {
            try {
              if (!deck || !deck.id || !deck.data) {
                reject('No deck found');
                return;
              }

              await this.saveSlides(deck);

              await set(`/decks/${deck.id}`, deck);

              resolve();
            } catch (err) {
              reject(err);
            }
          });
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

      resolve(slide);
    });
  }

  private uploadData(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      try {
        this.deckEditorService
          .watch()
          .pipe(take(1))
          .subscribe(async (deck: Deck) => {
            try {
              if (!deck || !deck.id || !deck.data) {
                reject('No deck found');
                return;
              }

              await this.uploadSlides(deck);

              const persistedDeck: Deck = await this.uploadDeck(deck);

              this.deckEditorService.next(persistedDeck);

              resolve();
            } catch (err) {
              reject(err);
            }
          });
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

  private uploadSlide(deck: Deck, slideId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const slide: Slide = await get(`/decks/${deck.id}/slides/${slideId}`);

        if (!slide || !slide.data) {
          reject('Missing slide for upload');
          return;
        }

        slide.data.attributes = await OfflineUtils.prepareAttributes(slide.data.attributes);

        await this.slideOnlineService.update(deck.id, slide);

        await del(`/decks/${deck.id}/slides/${slideId}`);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  private uploadDeck(deck: Deck): Promise<Deck> {
    return new Promise<Deck>(async (resolve, reject) => {
      try {
        if (!deck || !deck.data) {
          reject('Missing deck for upload');
          return;
        }

        deck.data.attributes = await OfflineUtils.prepareAttributes(deck.data.attributes);

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
}
