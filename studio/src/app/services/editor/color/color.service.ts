import {DEFAULT_PALETTE, StyloPalette} from '@papyrs/stylo';
import {get} from 'idb-keyval';
import colorStore from '../../../stores/color.store';

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
      const history = await get<StyloPalette[]>('deckdeckgo_color_history');
      colorStore.state.history = history ?? DEFAULT_PALETTE;

      const colorInput: 'hex' | 'rgb' = await get<'hex' | 'rgb'>('deckdeckgo_color_input');
      colorStore.state.colorInput = colorInput ?? 'hex';
    } catch (err) {
      console.warn(`Couldn't find stored palette. Proceeding with default`);
      colorStore.state.history = DEFAULT_PALETTE;
      colorStore.state.colorInput = 'hex';
    }
  }
}
