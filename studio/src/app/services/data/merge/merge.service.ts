import {Deck} from '../../../models/data/deck';

import { getMany, keys } from 'idb-keyval';

import { Slide } from '../../../models/data/slide';

import { DeckService } from '../deck/deck.service';
import { SlideOnlineService } from '../slide/slide.online.service';

export class MergeService {
  private static instance: MergeService;

  private deckService: DeckService;
  private slideOnlineService: SlideOnlineService;

  private constructor() {
    // Private constructor, singleton
    this.deckService = DeckService.getInstance();
    this.slideOnlineService = SlideOnlineService.getInstance();
  }

  static getInstance() {
    if (!MergeService.instance) {
      MergeService.instance = new MergeService();
    }
    return MergeService.instance;
  }

  mergeDeck(deckId: string, newUserId: string): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {

      // TODO: test and finish implementation

      if (!deckId || !newUserId) {
        resolve();
        return;
      }

      try {
        const deck: Deck = await this.deckService.get(deckId);

        if (!deck || !deck.data) {
          resolve();
          return;
        }

        deck.data.owner_id = newUserId;

        await this.deckService.update(deck);

        // TODO: performance -> should we save which slide sync and which not?

        const allKeys: string[] | undefined = await keys();

        const slideKeys: string[] = allKeys?.filter((key: string) => key.indexOf(`/decks/${deckId}/slides/`) > -1);

        const slides: Slide[] | undefined = await getMany(slideKeys);

        const promises: Promise<void>[] = slides?.map((slide: Slide) => this.slideOnlineService.update(deckId, slide));

        if (!promises) {
          // TODO error
          await Promise.all(promises);
        }

        resolve();
      } catch (err) {
        reject(err);
      }
    });
  }
}
