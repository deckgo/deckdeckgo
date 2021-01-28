import settingsStore from '../../stores/settings.store';

import {SettingsPanels} from '../../types/core/settings';

export class SettingsUtils {
  public static update(panels: Partial<SettingsPanels>) {
    settingsStore.state.panels = {
      ...settingsStore.state.panels,
      ...panels,
    };
  }
}
