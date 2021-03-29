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
      const lang: Languages | null = await get<Languages>('deckdeckgo_lang');

      if (lang) {
        i18n.state.lang = lang;
        return;
      }

      this.initDefaultLang();
    } catch (err) {
      console.warn(`Couldn't find lang. Proceeding with default`);
    }
  }

  private initDefaultLang() {
    const browserLang: string | undefined = this.getBrowserLang();
    i18n.state.lang = /(es|en|de)/gi.test(browserLang) ? (browserLang as Languages) : 'en';
  }

  /**
   * From ngx-translate
   * https://github.com/ngx-translate/core/blob/efcb4f43a645d9ac630aae8e50b60cc883e675fd/projects/ngx-translate/core/src/lib/translate.service.ts
   * @private
   */
  private getBrowserLang(): string | undefined {
    if (typeof window === 'undefined' || typeof window.navigator === 'undefined') {
      return undefined;
    }

    let browserLang: string | null = window.navigator.languages ? window.navigator.languages[0] : null;
    // @ts-ignore
    browserLang = browserLang || window.navigator.language || window.navigator.browserLanguage || window.navigator.userLanguage;

    if (typeof browserLang === 'undefined') {
      return undefined;
    }

    if (browserLang.indexOf('-') !== -1) {
      browserLang = browserLang.split('-')[0];
    }

    if (browserLang.indexOf('_') !== -1) {
      browserLang = browserLang.split('_')[0];
    }

    return browserLang;
  }
}
