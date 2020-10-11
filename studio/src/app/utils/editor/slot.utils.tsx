import {SlotType} from './slot-type';

export class SlotUtils {
  static isNodeReveal(selectedElement: HTMLElement): boolean {
    return selectedElement?.nodeName?.toLowerCase() === SlotType.REVEAL;
  }

  static isNodeRevealList(selectedElement: HTMLElement): boolean {
    return selectedElement?.nodeName?.toLowerCase() === SlotType.REVEAL_LIST;
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

  static isNodeImage(selectedElement: HTMLElement | null): boolean {
    return selectedElement?.nodeName?.toLowerCase() === SlotType.IMG;
  }

  static isNodeSocial(selectedElement: HTMLElement | null): boolean {
    return selectedElement?.nodeName?.toLowerCase() === SlotType.SOCIAL;
  }

  static isNodeCode(selectedElement: HTMLElement | null): boolean {
    return selectedElement?.nodeName?.toLowerCase() === SlotType.CODE;
  }

  static isNodeWordCloud(selectedElement: HTMLElement | null): boolean {
    return selectedElement?.nodeName?.toLowerCase() === SlotType.WORD_CLOUD;
  }

  static isSlotTypeEditable(type: SlotType): boolean {
    return type !== SlotType.IMG && type !== SlotType.SOCIAL && type !== SlotType.DEMO;
  }

  static isNodeDragDropResize(selectedElement: HTMLElement | null): boolean {
    return selectedElement?.nodeName?.toLowerCase() === SlotType.DRAG_RESIZE_ROTATE;
  }
}
