import {Component, h, State} from '@stencil/core';

import {set, get} from 'idb-keyval';

@Component({
  tag: 'app-theme-switcher',
  styleUrl: 'app-theme-switcher.scss',
})
export class AppThemeSwitcher {
  @State()
  private darkModePreference: boolean = false;

  private domBodyClassList: DOMTokenList = document.body.classList;

  private async initDarkModePreference(): Promise<boolean> {
    try {
      const savedDarkModePreference: boolean = await get('deckdeckgo_dark_mode');

      // If user already specified once a preference, we use that as default
      if (savedDarkModePreference !== undefined) {
        return savedDarkModePreference;
      }
    } catch (err) {
      return false;
    }

    // Otherwise we check the prefers-color-scheme of the OS
    const darkModePreferenceFromMedia: MediaQueryList = window.matchMedia('(prefers-color-scheme: dark)');

    return darkModePreferenceFromMedia.matches;
  }

  private updateDarkModePreferences() {
    !this.darkModePreference ? this.domBodyClassList.add('dark') : this.domBodyClassList.remove('dark');
    this.darkModePreference = !this.darkModePreference;
  }

  async componentWillLoad() {
    this.darkModePreference = await this.initDarkModePreference();

    if (this.darkModePreference) {
      this.domBodyClassList.add('dark');
    }
  }

  private async toggleTheme() {
    this.updateDarkModePreferences();

    try {
      await set('deckdeckgo_dark_mode', this.darkModePreference);
    } catch (err) {
      // We ignore this error. In worst case scenario, the application will be displayed in another theme after next refresh.
    }
  }

  render() {
    return <ion-toggle checked={this.darkModePreference} mode="md" color="switcher" onIonChange={() => this.toggleTheme()}></ion-toggle>;
  }
}
