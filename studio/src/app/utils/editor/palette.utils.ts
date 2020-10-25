import {DeckdeckgoPalette, DeckdeckgoPaletteColor} from '@deckdeckgo/color';

import colorStore from '../../stores/color.store';

export class PaletteUtils {
  static async updatePalette(color: DeckdeckgoPaletteColor) {
    if (colorStore.state.palette.some((palette: DeckdeckgoPalette) => palette.color.hex === color.hex)) {
      return;
    }

    colorStore.state.palette = [{color}, ...colorStore.state.palette];
  }
}
