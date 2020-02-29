import {firebase} from '@firebase/app';

import {del, get, set} from 'idb-keyval';

import {take} from 'rxjs/operators';

import {Deck} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';

import {SlotType} from '../../../utils/editor/slot-type';

import {DeckEditorService} from '../deck/deck-editor.service';
import {SlideOnlineService} from '../../data/slide/slide.online.service';

export class OfflineService {
  private static instance: OfflineService;

  private deckEditorService: DeckEditorService;
  private slideOnlineService: SlideOnlineService;

  private constructor() {
    this.deckEditorService = DeckEditorService.getInstance();
    this.slideOnlineService = SlideOnlineService.getInstance();
  }

  static getInstance() {
    if (!OfflineService.instance) {
      OfflineService.instance = new OfflineService();
    }
    return OfflineService.instance;
  }

  save(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const promises: Promise<void>[] = [this.saveDeck(), this.lazyLoadAllContent()];

        await Promise.all(promises);

        await this.addToSWCache();

        await set('deckdeckgo_offline', true);

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }

  upload(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        await this.uploadDeck();

        await del('deckdeckgo_offline');

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

  private uploadDeck(): Promise<void> {
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

              // TODO upload deck

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
        const promises: Promise<Slide>[] = [];

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

  private uploadSlide(deck: Deck, slideId: string): Promise<Slide> {
    return new Promise<Slide>(async (resolve, reject) => {
      const slide: Slide = await get(`/decks/${deck.id}/slides/${slideId}`);

      if (!slide || !slide.data) {
        reject('Missing slide for publishing');
        return;
      }

      const slideToPersist: Slide = await this.cleanSlide(slide);

      await this.slideOnlineService.update(deck.id, slideToPersist);

      await del(`/decks/${deck.id}/slides/${slideId}`);

      resolve(slide);
    });
  }

  private async cleanSlide(slide: Slide): Promise<Slide> {
    if (!slide || !slide.data || !slide.data.attributes) {
      return slide;
    }

    const keys: string[] = Object.keys(slide.data.attributes);

    if (!keys || keys.length <= 0) {
      return slide;
    }

    keys.forEach((key: string) => {
      const attr = slide.data.attributes[key];

      // Replace null values with Firestore "to delete fields"
      if (attr === null) {
        // @ts-ignore
        slide.data.attributes[key] = firebase.firestore.FieldValue.delete();
      }
    });

    console.log(slide);

    return slide;
  }
}
