import {SlotType} from './slot-type';
import {SlotUtils} from './slot.utils';
import {ListStyle, mapHtmlListStyleTypeToListStyle} from './list-style-type';

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
      const listStyle = mapHtmlListStyleTypeToListStyle(element.style.listStyleType);
      return listStyle === undefined ? (element.nodeName.toLowerCase() === SlotType.OL ? ListStyle.DECIMAL : ListStyle.BULLET) : listStyle;
    }
  }
}
