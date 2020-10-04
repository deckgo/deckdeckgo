import {SlotType} from './slot-type';
import {SlotUtils} from './slot.utils';
import {OrderedStyle, UnorderedStyle, mapHtmlListStyleTypeToListStyle} from './list-style-type';

export class ListUtils {
  static async isElementList(element: HTMLElement): Promise<SlotType.OL | SlotType.UL | undefined> {
    if (!SlotUtils.isNodeList(element)) {
      return undefined;
    }

    if (SlotUtils.isNodeRevealList(element)) {
      return element && element.getAttribute('list-tag') === SlotType.UL ? SlotType.UL : SlotType.OL;
    } else {
      return element && element.nodeName && element.nodeName.toLowerCase() === SlotType.OL ? SlotType.OL : SlotType.UL;
    }
  }

  static async getListElementType(element: HTMLElement): Promise<OrderedStyle | UnorderedStyle | undefined> {
    if (!SlotUtils.isNodeList(element)) {
      return undefined;
    }

    if (
      (element && element.nodeName && element.nodeName.toLowerCase() === SlotType.OL) ||
      (element && element.nodeName && element.nodeName.toLowerCase() === SlotType.UL)
    ) {
      const listStyle = mapHtmlListStyleTypeToListStyle(element.style.listStyleType);
      return listStyle === undefined ? (element.nodeName.toLowerCase() === SlotType.OL ? OrderedStyle.DECIMAL : UnorderedStyle.BULLET) : listStyle;
    }
  }
}
