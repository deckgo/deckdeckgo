import {Slide, SlideData} from '../../../models/data/slide';

import {SlideOfflineService} from './slide.offline.service';
import {SlideOnlineService} from './slide.online.service';

import authStore from '../../../stores/auth.store';

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

  async create(deckId: string, slideData: SlideData): Promise<Slide> {
    const slide: Slide = await SlideOfflineService.getInstance().create(deckId, slideData);

    if (navigator.onLine && authStore.state.loggedIn) {
      await SlideOnlineService.getInstance().update(deckId, slide);
    }

    return slide;
  }

  async update(deckId: string, slide: Slide): Promise<void> {
    const updatedSlide: Slide = await SlideOfflineService.getInstance().update(deckId, slide);

    if (navigator.onLine && authStore.state.loggedIn) {
      await SlideOnlineService.getInstance().update(deckId, updatedSlide);
    }
  }

  async delete(deckId: string, slideId: string): Promise<void> {
    await SlideOfflineService.getInstance().delete(deckId, slideId);

    if (navigator.onLine && authStore.state.loggedIn) {
      await SlideOnlineService.getInstance().delete(deckId, slideId);
    }
  }

  async get(deckId: string, slideId: string): Promise<Slide> {
    return SlideOfflineService.getInstance().get(deckId, slideId);
  }
}
