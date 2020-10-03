import paletteStore from '../../stores/palette.store';

import {get} from 'idb-keyval';

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
      paletteStore.state.palette = palette ?? DEFAULT_PALETTE;
    } catch (err) {
      console.warn("Couldn't find stored palette. Proceeding with default");
      paletteStore.state.palette = DEFAULT_PALETTE;
    }
  }
}
