import {Component, Fragment, h} from '@stencil/core';

import themeStore from '../../../../stores/theme.store';
import settingsStore from '../../../../stores/settings.store';

import {ThemeService} from '../../../../services/theme/theme.service';

import {EditMode} from '../../../../types/core/settings';

@Component({
  tag: 'app-customization',
  styleUrl: 'app-customization.scss',
})
export class AppCustomization {
  private themeService: ThemeService;

  private editMode: EditMode = settingsStore.state.editMode;

  constructor() {
    this.themeService = ThemeService.getInstance();
  }

  private async toggleTheme() {
    await this.themeService.switch(!themeStore.state.darkTheme);
  }

  private toggleEditMode() {
    settingsStore.state.editMode = settingsStore.state.editMode === 'css' ? 'properties' : 'css';
  }

  render() {
    return [
      <app-navigation></app-navigation>,
      <ion-content class="ion-padding fullscreen-padding">
        <main class="ion-padding fit">
          <h1>Customization</h1>

          <ion-list class="inputs-list dark-light-list">
            {this.renderDarkLightToggle()}

            {this.renderEditMode()}
          </ion-list>
        </main>
      </ion-content>,
    ];
  }

  private renderDarkLightToggle() {
    return (
      <ion-item>
        <ion-label>
          {themeStore.state.darkTheme ? 'Dark' : 'Light'} theme {themeStore.state.darkTheme ? '🌑' : '☀️'}
        </ion-label>
        <ion-toggle slot="end" checked={themeStore.state.darkTheme} mode="md" color="medium" onIonChange={async () => await this.toggleTheme()}></ion-toggle>
      </ion-item>
    );
  }

  private renderEditMode() {
    return (
      <Fragment>
        <ion-item-divider class="ion-padding-top">
          <ion-label>Edit mode</ion-label>
        </ion-item-divider>

        <ion-radio-group value={this.editMode} onIonChange={() => this.toggleEditMode()}>
          <ion-item>
            <ion-radio value="properties" mode="md" slot="start"></ion-radio>
            <ion-label>Properties</ion-label>
          </ion-item>

          <ion-item>
            <ion-radio value="css" mode="md" slot="start"></ion-radio>
            <ion-label>CSS</ion-label>
          </ion-item>
        </ion-radio-group>
      </Fragment>
    );
  }
}
