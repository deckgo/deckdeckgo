import {createStore} from '@stencil/store';

import {set as idbSet} from 'idb-keyval';

import {Settings, SettingsPanels} from '../types/core/settings';

const {state, onChange, set} = createStore<Settings>({
  panels: {
    borderRadius: 'close',
    boxShadow: 'close',
  },
});

onChange('panels', (panels: SettingsPanels) => {
  idbSet('deckdeckgo_settings_panels', panels).catch((err) => {
    console.error('Failed to update IDB with new panel settings', err);
  });
});

export default {state, set};
