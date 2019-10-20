import {Component, h, Listen, State} from '@stencil/core';

import {set, get} from 'idb-keyval';

@Component({
  tag: 'app-theme-switcher',
  styleUrl: 'app-theme-switcher.scss'
})
export class AppThemeSwitcher {

  @State()
  private darkModePreference: Boolean = false;

  private domBodyClassList: DOMTokenList = document.body.classList;

  // error handling if the idb-keyval throws an error
  private async idbKeyValPromiseHandler(promise): Promise<any> {
    return Promise.resolve(promise)
      .then(valueOfPromise => valueOfPromise)
      .catch(err => Error(err));
  }

  private async resolveDarkModePreference(): Promise<Boolean> {
    let getDarkModePreferenceFromIdb: Promise<any> = await this.idbKeyValPromiseHandler(get('darkModePreferences'));
    let getDarkModePreferenceFromMedia = window.matchMedia('(prefers-color-scheme: dark)');
    if (getDarkModePreferenceFromIdb instanceof Error) {
      return false;
    }
    let isDarkModePreferenceDetermined: Boolean | Promise<Boolean> =
      getDarkModePreferenceFromIdb ||
      getDarkModePreferenceFromMedia.matches;

    return isDarkModePreferenceDetermined ? isDarkModePreferenceDetermined : this.darkModePreference;
  }


  private updateDarkModePreferences(): void {
    !this.darkModePreference ?
      this.domBodyClassList.add('dark') :
      this.domBodyClassList.remove('dark');
    this.darkModePreference = !this.darkModePreference;
  }

  async componentDidLoad() {
    this.darkModePreference = await this.resolveDarkModePreference();
    const DARKTOGGLE: HTMLInputElement = document.querySelector('#themeToggle');
    if (this.darkModePreference) {
      DARKTOGGLE.checked = true;
      this.domBodyClassList.add('dark');
    }
  }

  @Listen('click', {capture: true})
  handleOnToggleChange() {
    this.updateDarkModePreferences();
    this.idbKeyValPromiseHandler(set('darkModePreferences', this.darkModePreference));
  }

  render() {
    return <ion-toggle id="themeToggle" mode="md" color="switcher"></ion-toggle>;
  }
}
