import {SlideSplitType} from '@deckdeckgo/editor';

import {isSlide} from '@deckdeckgo/deck-utils';

import {SlotType} from '../../types/editor/slot-type';
import {SelectedTarget} from '../../types/editor/selected-target';

import {SlotUtils} from './slot.utils';
import {ListUtils} from './list.utils';
import {SlideUtils} from './slide.utils';

export class SelectedElementUtils {
  static isElementSlide(element: HTMLElement): 'slide' | 'element' {
    return isSlide(element) ? 'slide' : 'element';
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

  private static isElementWordCloud(element: HTMLElement): boolean {
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

  static initDescription(element: HTMLElement): Partial<SelectedTarget> {
    const type: 'element' | 'slide' = this.isElementSlide(element);

    if (type === 'element') {
      return {
        type,
        element: {
          math: this.isElementMath(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element),
          wordCloud: this.isElementWordCloud(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element),
          markdown: this.isElementMarkdown(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element),
          code: this.isElementCode(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element),
          image: this.isElementImage(SlotUtils.isNodeReveal(element) ? (element.firstElementChild as HTMLElement) : element),
          shape: this.isElementShape(element),
          demo: this.isElementDemo(element),
          list: ListUtils.isElementList(element)
        }
      };
    }

    const nodeName: string | undefined = element?.nodeName.toLowerCase();

    return {
      type,
      slide: {
        nodeName,
        scope: SlideUtils.slideScope(element),
        demo: nodeName === 'deckgo-slide-split' && element?.getAttribute('type') === SlideSplitType.DEMO,
        qrCode: nodeName === 'deckgo-slide-qrcode',
        chart: nodeName === 'deckgo-slide-chart',
        author: nodeName === 'deckgo-slide-author',
        aspectRatio: nodeName === 'deckgo-slide-aspect-ratio',
        poll: nodeName === 'deckgo-slide-poll',
        split: nodeName === 'deckgo-slide-split' && element?.getAttribute('type') !== SlideSplitType.DEMO,
        youtube: nodeName === 'deckgo-slide-youtube',
        playground: nodeName === 'deckgo-slide-playground',
        fixed: ['deckgo-slide-title', 'deckgo-slide-content', 'deckgo-slide-split'].includes(nodeName)
      }
    };
  }
}
