import {StyloPalette} from '@papyrs/stylo';
import {createStore} from '@stencil/store';
import {set} from 'idb-keyval';

interface ColorStore {
  history: StyloPalette[];
  colorInput: 'hex' | 'rgb';
}

const {state, onChange} = createStore<ColorStore>({
  history: [],
  colorInput: 'hex'
});

onChange('history', (history: StyloPalette[]) => {
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
