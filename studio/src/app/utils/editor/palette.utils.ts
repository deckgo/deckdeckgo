import {DeckdeckgoPalette, DeckdeckgoPaletteColor} from '@deckdeckgo/color';

import colorStore from '../../stores/color.store';

export class PaletteUtils {
  static async updatePalette(color: DeckdeckgoPaletteColor) {
    const filteredPalette: DeckdeckgoPalette[] = colorStore.state.palette.filter((palette: DeckdeckgoPalette) => palette.color.hex !== color.hex);

    colorStore.state.palette = [{color}, ...(filteredPalette.length < 22 ? filteredPalette : filteredPalette.slice(0, 21))];
  }
}
