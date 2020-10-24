import {createStore} from '@stencil/store';

import {set} from 'idb-keyval';

import {DeckdeckgoPalette} from '@deckdeckgo/color';

interface PaletteStore {
  palette: DeckdeckgoPalette[];
  colorInput: 'hex' | 'rgb';
}

const {state, onChange} = createStore<PaletteStore>({
  palette: [],
  colorInput: 'hex',
});

onChange('palette', (palette: DeckdeckgoPalette[]) => {
  set('deckdeckgo_palette', palette).catch((err) => {
    console.error('Failed to update IDB with new palette', err);
  });
});

onChange('colorInput', (colorInput: 'hex' | 'rgb') => {
  set('deckdeckgo_color_input', colorInput).catch((err) => {
    console.error('Failed to update IDB with new color input type', err);
  });
});

export default {state, onChange};
