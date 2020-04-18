import {SlotType} from './slot-type';
import {SlotUtils} from './slot.utils';

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
}
