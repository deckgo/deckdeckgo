import {Component, h} from '@stencil/core';

import themeStore from '../../../stores/theme.store';

import {ThemeService} from '../../../services/theme/theme.service';

@Component({
  tag: 'app-general-settings'
})
export class AppGeneralSettings {
  private themeService: ThemeService;

  constructor() {
    this.themeService = ThemeService.getInstance();
  }

  async toggleTheme() {
    await this.themeService.switch(!themeStore.state.darkTheme);
  }

  render() {
    return [
      <h1 class="ion-padding-top">Settings</h1>,
      <ion-list class="ion-padding-top ion-padding-bottom">
        <ion-item>
          <ion-label>
            {themeStore.state.darkTheme ? 'Dark' : 'Light'} theme {themeStore.state.darkTheme ? '🌑' : '☀️'}
          </ion-label>
          <ion-toggle
            slot="end"
            checked={themeStore.state.darkTheme}
            mode="md"
            color="switcher"
            onIonChange={() => this.toggleTheme()}></ion-toggle>
        </ion-item>
      </ion-list>
    ];
  }
}
