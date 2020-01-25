import {Observable, ReplaySubject} from 'rxjs';

import {get, set} from 'idb-keyval';

export class ThemeService {
  private static instance: ThemeService;

  private darkTheme: ReplaySubject<boolean> = new ReplaySubject<boolean>(1);

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!ThemeService.instance) {
      ThemeService.instance = new ThemeService();
    }
    return ThemeService.instance;
  }

  watch(): Observable<boolean> {
    return this.darkTheme.asObservable();
  }

  async switch(dark: boolean) {
    this.darkTheme.next(dark);

    try {
      await set('deckdeckgo_dark_mode', dark);
    } catch (err) {
      // We ignore this error. In worst case scenario, the application will be displayed in another theme after next refresh.
    }
  }

  async initDarkModePreference(): Promise<void> {
    try {
      const savedDarkModePreference: boolean = await get('deckdeckgo_dark_mode');

      // If user already specified once a preference, we use that as default
      if (savedDarkModePreference !== undefined) {
        this.switch(savedDarkModePreference);
        return;
      }
    } catch (err) {
      this.switch(false);
      return;
    }

    // Otherwise we check the prefers-color-scheme of the OS
    const darkModePreferenceFromMedia: MediaQueryList = window.matchMedia('(prefers-color-scheme: dark)');

    this.switch(darkModePreferenceFromMedia.matches);
  }
}
