import {createStore} from '@stencil/store';

import {set} from 'idb-keyval';

import {DeckdeckgoPalette} from '@deckdeckgo/color';

interface ColorStore {
  history: DeckdeckgoPalette[];
  palette: DeckdeckgoPalette[];
  colorInput: 'hex' | 'rgb';
}

const {state, onChange} = createStore<ColorStore>({
  history: [],
  palette: [],
  colorInput: 'hex',
});

onChange('history', (history: DeckdeckgoPalette[]) => {
  set('deckdeckgo_color_history', history).catch((err) => {
    console.error('Failed to update IDB with new color history', err);
  });
});

onChange('palette', (palette: DeckdeckgoPalette[]) => {
  set('deckdeckgo_color_palette', palette).catch((err) => {
    console.error('Failed to update IDB with new color palette', err);
  });
});

onChange('colorInput', (colorInput: 'hex' | 'rgb') => {
  set('deckdeckgo_color_input', colorInput).catch((err) => {
    console.error('Failed to update IDB with new color input type', err);
  });
});

export default {state, onChange};
