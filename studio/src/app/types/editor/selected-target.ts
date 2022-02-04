import {SlideScope} from '@deckdeckgo/editor';

import {SlotType} from './slot-type';

export interface SelectedElement {
  code: boolean;
  math: boolean;
  wordCloud: boolean;
  markdown: boolean;
  image: boolean;
  shape: 'shape' | 'text' | undefined;
  demo: boolean;
  list: SlotType.OL | SlotType.UL | undefined;
}

export interface SelectedSlide {
  nodeName: string | undefined;
  scope: SlideScope;
  demo: boolean;
  qrCode: boolean;
  chart: boolean;
  author: boolean;
  aspectRatio: boolean;
  poll: boolean;
  split: boolean;
  youtube: boolean;
  playground: boolean;
  fixed: boolean;
}

export interface SelectedTarget {
  target: HTMLElement;
  type: 'slide' | 'element';
  slide?: SelectedSlide;
  element?: SelectedElement;
}
