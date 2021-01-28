import {createStore} from '@stencil/store';

import {set} from 'idb-keyval';

import {Settings, SettingsPanels} from '../types/core/settings';

const {state, onChange} = createStore<Settings>({
  panels: {
    borderRadius: 'close',
    boxShadow: 'close',
    align: 'close',
    fontSize: 'open',
    letterSpacing: 'close',
    image: 'open',
    color: 'open',
    background: 'close',
  },
});

onChange('panels', (panels: SettingsPanels) => {
  set('deckdeckgo_settings_panels', panels).catch((err) => {
    console.error('Failed to update IDB with new panel settings', err);
  });
});

export default {state};
