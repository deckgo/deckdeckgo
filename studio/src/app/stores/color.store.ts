import {createStore} from '@stencil/store';

import {set} from 'idb-keyval';

import { DeckdeckgoPalette } from '../utils/ddg/deckdeckgo-palette';

interface ColorStore {
  history: DeckdeckgoPalette[];
  colorInput: 'hex' | 'rgb';
}

const {state, onChange} = createStore<ColorStore>({
  history: [],
  colorInput: 'hex'
});

onChange('history', (history: DeckdeckgoPalette[]) => {
  set('deckdeckgo_color_history', history).catch((err) => {
    console.error('Failed to update IDB with new color history', err);
  });
});

onChange('colorInput', (colorInput: 'hex' | 'rgb') => {
  set('deckdeckgo_color_input', colorInput).catch((err) => {
    console.error('Failed to update IDB with new color input type', err);
  });
});

export default {state, onChange};
