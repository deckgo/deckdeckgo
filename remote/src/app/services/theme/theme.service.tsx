import themeStore from '../../stores/theme.store';

import {get, set} from 'idb-keyval';

export class ThemeService {
  private static instance: ThemeService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!ThemeService.instance) {
      ThemeService.instance = new ThemeService();
    }
    return ThemeService.instance;
  }

  async switch(dark: boolean) {
    themeStore.state.darkTheme = dark;

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
        await this.switch(savedDarkModePreference);
        return;
      }
    } catch (err) {
      await this.switch(false);
      return;
    }

    // Otherwise we check the prefers-color-scheme of the OS
    const darkModePreferenceFromMedia: MediaQueryList = window.matchMedia('(prefers-color-scheme: dark)');

    await this.switch(darkModePreferenceFromMedia.matches);
  }
}
