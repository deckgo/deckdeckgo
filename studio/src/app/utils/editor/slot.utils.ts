import {SlotType} from '../../types/editor/slot-type';

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

  static isNodeMarkdown(selectedElement: HTMLElement | null): boolean {
    return selectedElement?.nodeName?.toLowerCase() === SlotType.MARKDOWN;
  }

  static isSlotTypeEditable(type: SlotType): boolean {
    return type !== SlotType.IMG && type !== SlotType.SOCIAL && type !== SlotType.DEMO;
  }

  static isNodeDragDropResize(selectedElement: HTMLElement | null): boolean {
    return selectedElement?.nodeName?.toLowerCase() === SlotType.DRAG_RESIZE_ROTATE;
  }

  static isNodeEditable(element?: HTMLElement): boolean {
    return (
      element?.nodeName?.toLowerCase() === SlotType.CODE ||
      element?.nodeName?.toLowerCase() === SlotType.MATH ||
      element?.nodeName?.toLowerCase() === SlotType.WORD_CLOUD ||
      element?.nodeName?.toLowerCase() === SlotType.MARKDOWN
    );
  }

  static isNodeTitle(selectedElement: HTMLElement): boolean {
    return [SlotType.H1.toString(), SlotType.H2.toString(), SlotType.H3.toString()].includes(selectedElement.nodeName.toLowerCase());
  }
}
