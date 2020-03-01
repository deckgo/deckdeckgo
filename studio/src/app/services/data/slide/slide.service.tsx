import {Slide, SlideData} from '../../../models/data/slide';

import {SlideOfflineService} from './slide.offline.service';
import {SlideOnlineService} from './slide.online.service';
import {OfflineService} from '../../editor/offline/offline.service';

export class SlideService {
  private static instance: SlideService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!SlideService.instance) {
      SlideService.instance = new SlideService();
    }
    return SlideService.instance;
  }

  async create(deckId: string, slide: SlideData): Promise<Slide> {
    const offline: OfflineDeck = await OfflineService.getInstance().status();

    if (offline !== undefined) {
      return SlideOfflineService.getInstance().create(deckId, slide);
    } else {
      return SlideOnlineService.getInstance().create(deckId, slide);
    }
  }

  async update(deckId: string, slide: Slide): Promise<void> {
    const offline: OfflineDeck = await OfflineService.getInstance().status();

    if (offline !== undefined) {
      return SlideOfflineService.getInstance().update(deckId, slide);
    } else {
      return SlideOnlineService.getInstance().update(deckId, slide);
    }
  }

  async delete(deckId: string, slideId: string): Promise<void> {
    const offline: OfflineDeck = await OfflineService.getInstance().status();

    if (offline !== undefined) {
      return SlideOfflineService.getInstance().delete(deckId, slideId);
    } else {
      return SlideOnlineService.getInstance().delete(deckId, slideId);
    }
  }

  async get(deckId: string, slideId: string): Promise<Slide> {
    const offline: OfflineDeck = await OfflineService.getInstance().status();

    if (offline !== undefined) {
      return SlideOfflineService.getInstance().get(deckId, slideId);
    } else {
      return SlideOnlineService.getInstance().get(deckId, slideId);
    }
  }
}
