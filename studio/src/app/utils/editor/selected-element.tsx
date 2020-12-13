import {SlideSplitType} from '../../models/data/slide';

import {SlotType} from './slot-type';
import {SlotUtils} from './slot.utils';
import {ListUtils} from './list.utils';

export interface SelectedSlotDescription {
  code: boolean;
  math: boolean;
  wordCloud: boolean;
  markdown: boolean;
  image: boolean;
  shape: 'shape' | 'text' | undefined;
  demo: boolean;
  list: SlotType.OL | SlotType.UL | undefined;
}

export interface SelectedSlideDescription {
  nodeName: string | undefined;
  demo: boolean;
  qrCode: boolean;
  chart: boolean;
  author: boolean;
  aspectRatio: boolean;
  poll: boolean;
  split: boolean;
  youtube: boolean;
  playground: boolean;
}

export interface SelectedElementDescription {
  type: 'slide' | 'element'; // true = is a slide, false = it's an element / slot
  slide: SelectedSlideDescription;
  slot: SelectedSlotDescription;
}

export class SelectedElement {
  static isElementSlide(element: HTMLElement): 'slide' | 'element' {
    return element?.nodeName?.toLowerCase().indexOf('deckgo-slide') > -1 ? 'slide' : 'element';
  }

  private static isElementDemo(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.DEMO;
  }

  private static isElementCode(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.CODE;
  }

  private static isElementMath(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.MATH;
  }

  private static isElementWordcloud(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.WORD_CLOUD;
  }

  private static isElementMarkdown(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.MARKDOWN;
  }

  private static isElementShape(element: HTMLElement): 'shape' | 'text' | undefined {
    if (!element || !element.nodeName || element.nodeName.toLowerCase() !== SlotType.DRAG_RESIZE_ROTATE) {
      return undefined;
    }

    return element.hasAttribute('text') ? 'text' : 'shape';
  }

  private static isElementImage(element: HTMLElement): boolean {
    return element?.nodeName?.toLowerCase() === SlotType.IMG;
  }

  static initDescription(element: HTMLElement | undefined): SelectedElementDescription {
    let options: Partial<SelectedElementDescription> = {
      type: this.isElementSlide(element),
      slot: {
        math: this.isElementMath(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element),
        wordCloud: this.isElementWordcloud(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element),
        markdown: this.isElementMarkdown(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element),
        code: this.isElementCode(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element),
        image: this.isElementImage(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element),
        shape: this.isElementShape(element),
        demo: this.isElementDemo(element),
        list: ListUtils.isElementList(element),
      },
    };

    const nodeName: string | undefined = options.type === 'slide' ? element?.nodeName.toLowerCase() : undefined;

    return {
      ...(options as SelectedElementDescription),
      slide: {
        nodeName,
        demo: options.type === 'slide' && nodeName === 'deckgo-slide-split' && element?.getAttribute('type') === SlideSplitType.DEMO,
        qrCode: options.type === 'slide' && nodeName === 'deckgo-slide-qrcode',
        chart: options.type === 'slide' && nodeName === 'deckgo-slide-chart',
        author: options.type === 'slide' && nodeName === 'deckgo-slide-author',
        aspectRatio: options.type === 'slide' && nodeName === 'deckgo-slide-aspect-ratio',
        poll: options.type === 'slide' && nodeName === 'deckgo-slide-poll',
        split: options.type === 'slide' && nodeName === 'deckgo-slide-split' && element?.getAttribute('type') !== SlideSplitType.DEMO,
        youtube: options.type === 'slide' && nodeName === 'deckgo-slide-youtube',
        playground: options.type === 'slide' && nodeName === 'deckgo-slide-playground',
      },
    };
  }
}
