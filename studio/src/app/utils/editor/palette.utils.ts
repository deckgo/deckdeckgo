import {DeckdeckgoPalette, DeckdeckgoPaletteColor} from '@deckdeckgo/color';

import paletteStore from '../../stores/palette.store';

export class PaletteUtils {
  static async updatePalette(color: DeckdeckgoPaletteColor) {
    if (paletteStore.state.palette.some((palette: DeckdeckgoPalette) => palette.color.hex === color.hex)) {
      return;
    }

    paletteStore.state.palette = [{color}, ...paletteStore.state.palette];
  }
}
