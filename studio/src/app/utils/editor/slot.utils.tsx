import {SlotType} from './slot-type';

export class SlotUtils {
  static isNodeReveal(selectedElement: HTMLElement): boolean {
    return selectedElement.nodeName?.toLowerCase() === SlotType.REVEAL;
  }

  static isNodeRevealList(selectedElement: HTMLElement): boolean {
    return selectedElement.nodeName?.toLowerCase() === SlotType.REVEAL_LIST;
  }

  static isNodeList(selectedElement: HTMLElement): boolean {
    return (
      selectedElement &&
      selectedElement.nodeName &&
      (selectedElement.nodeName.toLowerCase() === SlotType.OL ||
        selectedElement.nodeName.toLowerCase() === SlotType.UL ||
        selectedElement.nodeName.toLowerCase() === SlotType.REVEAL_LIST)
    );
  }

  static isNodeImage(selectedElement: HTMLElement): boolean {
    return selectedElement.nodeName?.toLowerCase() === SlotType.IMG;
  }

  static isNodeSocial(selectedElement: HTMLElement): boolean {
    return selectedElement.nodeName?.toLowerCase() === SlotType.SOCIAL;
  }

  static isSlotTypeEditable(type: SlotType): boolean {
    return type !== SlotType.IMG && type !== SlotType.SOCIAL && type !== SlotType.DEMO;
  }

  static isNodeDragDropResize(selectedElement: HTMLElement): boolean {
    return selectedElement && selectedElement.nodeName && selectedElement.nodeName.toLowerCase() === SlotType.DRAG_RESIZE_ROTATE;
  }
}
