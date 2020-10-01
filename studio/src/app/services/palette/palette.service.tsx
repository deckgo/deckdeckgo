import paletteStore from '../../stores/palette.store';

import {get, set} from 'idb-keyval';
import {DeckdeckgoPalette, DEFAULT_PALETTE} from '@deckdeckgo/color';

export class PaletteService {
  private static instance: PaletteService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!PaletteService.instance) {
      PaletteService.instance = new PaletteService();
    }
    return PaletteService.instance;
  }
  async init() {
    try {
      const palette = await get<DeckdeckgoPalette[]>('deckdeckgo_palette');
      paletteStore.state.palette = palette;
    } catch (err) {
      console.warn("Couldn't find stored palette. Proceeding with default");
      paletteStore.state.palette = DEFAULT_PALETTE;
    }
  }
  async updatePalette(palette: DeckdeckgoPalette[]) {
    paletteStore.state.palette = palette;

    try {
      await set('deckdeckgo_palette', palette);
    } catch (err) {
      console.error('Failed to update IDB with new palette');
    }
  }
}
