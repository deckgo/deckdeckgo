import {DeckdeckgoPalette} from '@deckdeckgo/color/dist/types/utils/deckdeckgo-palette';
import {createStore} from '@stencil/store';

interface PaletteStore {
  palette: DeckdeckgoPalette[];
}

const {state} = createStore<PaletteStore>({
  palette: [],
});

export default {state};
