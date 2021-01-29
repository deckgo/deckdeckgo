import settingsStore from '../../stores/settings.store';

import {get} from 'idb-keyval';

import {EditMode, SettingsPanels} from '../../types/core/settings';

export class SettingsService {
  private static instance: SettingsService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!SettingsService.instance) {
      SettingsService.instance = new SettingsService();
    }
    return SettingsService.instance;
  }

  async init() {
    try {
      const settingsPanels: SettingsPanels | null = await get<SettingsPanels>('deckdeckgo_settings_panels');

      if (settingsPanels) {
        settingsStore.state.panels = settingsPanels;
      }

      const edit: EditMode | null = await get<EditMode>('deckdeckgo_settings_edit_mode');

      settingsStore.state.editMode = edit ?? 'properties';
    } catch (err) {
      console.warn(`Couldn't find settings for panels. Proceeding with default`);
    }
  }
}
