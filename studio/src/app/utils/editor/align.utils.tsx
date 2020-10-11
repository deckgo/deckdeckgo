import {isRTL} from '@deckdeckgo/utils';
import {SlotType} from './slot-type';

export enum TextAlign {
  LEFT = 'left',
  CENTER = 'center',
  RIGHT = 'right',
}

export class AlignUtils {
  static async getAlignment(element: HTMLElement): Promise<TextAlign | undefined> {
    if (!element || (!this.isElementText(element) && !this.isShapeText(element))) {
      return undefined;
    }

    const style: CSSStyleDeclaration = window.getComputedStyle(element);

    if (style.textAlign === 'center') {
      return TextAlign.CENTER;
    } else if (style.textAlign === 'right') {
      return TextAlign.RIGHT;
    } else if (style.textAlign === 'left') {
      return TextAlign.LEFT;
    }

    return this.getDefaultAlignment();
  }

  private static getDefaultAlignment(): TextAlign {
    return isRTL() ? TextAlign.RIGHT : TextAlign.LEFT;
  }

  private static isShapeText(element: HTMLElement): boolean {
    return element && element.nodeName && element.nodeName.toLowerCase() && SlotType.DRAG_RESIZE_ROTATE && element.hasAttribute('text');
  }

  private static isElementText(element: HTMLElement): boolean {
    return (
      element &&
      element.nodeName &&
      [
        SlotType.SECTION.toString(),
        SlotType.H1.toString(),
        SlotType.H2.toString(),
        SlotType.H3.toString(),
        SlotType.OL.toString(),
        SlotType.UL.toString(),
        SlotType.REVEAL.toString(),
        SlotType.REVEAL_LIST.toString(),
        SlotType.MATH.toString(),
        SlotType.WORD_CLOUD.toString(),
      ].indexOf(element.nodeName.toLowerCase()) > -1
    );
  }
}
