import {set} from 'idb-keyval';

import {take} from 'rxjs/operators';

import {Deck} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';

import {DeckEditorService} from '../deck/deck-editor.service';
import {SlideService} from '../../data/slide/slide.service';

export class OfflineService {
  private static instance: OfflineService;

  private deckEditorService: DeckEditorService;
  private slideService: SlideService;

  private constructor() {
    this.deckEditorService = DeckEditorService.getInstance();
    this.slideService = SlideService.getInstance();
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

        // TODO: call SW cache images

        await set('deckdeckgo_offline', true);

        resolve();
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
      const slide: Slide = await this.slideService.get(deck.id, slideId);

      if (!slide || !slide.data) {
        reject('Missing slide for publishing');
        return;
      }

      await set(`/decks/${deck.id}/slides/${slideId}`, slide);

      resolve(slide);
    });
  }
}
