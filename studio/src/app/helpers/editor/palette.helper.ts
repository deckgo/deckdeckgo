import {DeckdeckgoPalette, DeckdeckgoPaletteColor} from '@deckdeckgo/color';

export function colorInPaletteHandler(currentPalette: DeckdeckgoPalette[], color: DeckdeckgoPaletteColor) {
  console.log(currentPalette, color);
  if (currentPalette) {
    if (currentPalette.some((palette) => palette.color.hex === color.hex)) return currentPalette;
    return [{color}, ...currentPalette];
  }
}
