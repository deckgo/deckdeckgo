import {SlotType} from '../../types/editor/slot-type';
import {SlotUtils} from './slot.utils';
import {ListStyle} from '../../types/editor/list-style';

export class ListUtils {
  static isElementList(element: HTMLElement): SlotType.OL | SlotType.UL | undefined {
    if (!SlotUtils.isNodeList(element)) {
      return undefined;
    }

    if (SlotUtils.isNodeRevealList(element)) {
      return element && element.getAttribute('list-tag') === SlotType.UL ? SlotType.UL : SlotType.OL;
    } else {
      return element?.nodeName?.toLowerCase() === SlotType.OL ? SlotType.OL : SlotType.UL;
    }
  }

  static getListElementType(element: HTMLElement): ListStyle | undefined {
    if (!SlotUtils.isNodeList(element)) {
      return undefined;
    }

    if (element?.nodeName?.toLowerCase() === SlotType.OL || element?.nodeName?.toLowerCase() === SlotType.UL) {
      const listStyle = this.mapHtmlListStyleTypeToListStyle(element.style.listStyleType);
      return listStyle === undefined ? (element.nodeName.toLowerCase() === SlotType.OL ? ListStyle.DECIMAL : ListStyle.BULLET) : listStyle;
    }
  }

  private static mapHtmlListStyleTypeToListStyle(listType: string): ListStyle | undefined {
    switch (listType) {
      case 'decimal':
        return ListStyle.DECIMAL;
      case 'decimal-leading-zero':
        return ListStyle.DECIMAL_LEADING;
      case 'upper-roman':
        return ListStyle.ROMAN_UPPER;
      case 'lower-roman':
        return ListStyle.ROMAN_LOWER;
      case 'lower-latin':
        return ListStyle.LATIN_LOWER;
      case 'upper-latin':
        return ListStyle.LATIN_UPPER;
      case 'disc':
        return ListStyle.BULLET;
      case 'circle':
        return ListStyle.CIRCLE;
      case 'square':
        return ListStyle.SQUARE;
      default:
        return undefined;
    }
  }
}
