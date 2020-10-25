import colorStore from '../../stores/color.store';

import {get} from 'idb-keyval';

import {DeckdeckgoPalette} from '@deckdeckgo/color';

export class ColorService {
  private static instance: ColorService;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!ColorService.instance) {
      ColorService.instance = new ColorService();
    }
    return ColorService.instance;
  }

  async init() {
    try {
      const palette = await get<DeckdeckgoPalette[]>('deckdeckgo_palette');
      colorStore.state.palette = palette ?? [];

      const colorInput: 'hex' | 'rgb' = await get<'hex' | 'rgb'>('deckdeckgo_color_input');
      colorStore.state.colorInput = colorInput ?? 'hex';
    } catch (err) {
      console.warn(`Couldn't find stored palette. Proceeding with default`);
      colorStore.state.palette = [];
      colorStore.state.colorInput = 'hex';
    }
  }
}
