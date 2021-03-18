import i18n from '../../stores/i18n.store';

import {get} from 'idb-keyval';

export class LangService {
  private static instance: LangService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!LangService.instance) {
      LangService.instance = new LangService();
    }
    return LangService.instance;
  }

  async init() {
    try {
      const lang: 'en' | 'es' | null = await get<'en' | 'es'>('deckdeckgo_lang');

      if (lang) {
        i18n.state.lang = lang;
      }
    } catch (err) {
      console.warn(`Couldn't find lang. Proceeding with default`);
    }
  }
}
