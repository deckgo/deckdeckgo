import {createStore} from '@stencil/store';

import {set} from 'idb-keyval';

import {EditMode, Settings, SettingsPanels} from '../types/core/settings';

const {state, onChange} = createStore<Settings>({
  panels: {
    borderRadius: 'close',
    boxShadow: 'close',
    block: 'close',
    fontSize: 'open',
    text: 'close',
    image: 'open',
    imageStyle: 'open',
    color: 'open',
    background: 'close',
    list: 'open',
  },
  editMode: 'properties',
});

onChange('panels', (panels: SettingsPanels) => {
  set('deckdeckgo_settings_panels', panels).catch((err) => {
    console.error('Failed to update IDB with new panel settings', err);
  });
});

onChange('editMode', (mode: EditMode) => {
  set('deckdeckgo_settings_edit_mode', mode).catch((err) => {
    console.error('Failed to update IDB with new edit mode', err);
  });
});

export default {state, onChange};
