import {Component, h} from '@stencil/core';

import themeStore from '../../../../stores/theme.store';

import {ThemeService} from '../../../../services/theme/theme.service';

@Component({
  tag: 'app-customization',
  styleUrl: 'app-customization.scss',
})
export class AppCustomization {
  private themeService: ThemeService;

  constructor() {
    this.themeService = ThemeService.getInstance();
  }

  async toggleTheme() {
    await this.themeService.switch(!themeStore.state.darkTheme);
  }

  render() {
    return [
      <app-navigation></app-navigation>,
      <ion-content class="ion-padding fullscreen-padding">
        <main class="ion-padding">{this.renderDarkLightToggle()}</main>
      </ion-content>,
    ];
  }

  private renderDarkLightToggle() {
    return [
      <h1>Customization</h1>,
      <ion-list class="inputs-list dark-light-list">
        <ion-item>
          <ion-label>
            {themeStore.state.darkTheme ? 'Dark' : 'Light'} theme {themeStore.state.darkTheme ? '🌑' : '☀️'}
          </ion-label>
          <ion-toggle slot="end" checked={themeStore.state.darkTheme} mode="md" color="medium" onIonChange={() => this.toggleTheme()}></ion-toggle>
        </ion-item>
      </ion-list>,
    ];
  }
}
