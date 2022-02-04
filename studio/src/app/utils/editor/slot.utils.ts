import {SlotType} from '../../types/editor/slot-type';

export class SlotUtils {
  static isNodeReveal(selectedTarget: HTMLElement): boolean {
    return selectedTarget?.nodeName?.toLowerCase() === SlotType.REVEAL;
  }

  static isNodeRevealList(selectedTarget: HTMLElement): boolean {
    return selectedTarget?.nodeName?.toLowerCase() === SlotType.REVEAL_LIST;
  }

  static isNodeList(selectedTarget: HTMLElement): boolean {
    return (
      selectedTarget &&
      selectedTarget.nodeName &&
      (selectedTarget.nodeName.toLowerCase() === SlotType.OL ||
        selectedTarget.nodeName.toLowerCase() === SlotType.UL ||
        selectedTarget.nodeName.toLowerCase() === SlotType.REVEAL_LIST)
    );
  }

  static isNodeImage(selectedTarget: HTMLElement | null): boolean {
    return selectedTarget?.nodeName?.toLowerCase() === SlotType.IMG;
  }

  static isNodeSocial(selectedTarget: HTMLElement | null): boolean {
    return selectedTarget?.nodeName?.toLowerCase() === SlotType.SOCIAL;
  }

  static isNodeCode(selectedTarget: HTMLElement | null): boolean {
    return selectedTarget?.nodeName?.toLowerCase() === SlotType.CODE;
  }

  static isNodeWordCloud(selectedTarget: HTMLElement | null): boolean {
    return selectedTarget?.nodeName?.toLowerCase() === SlotType.WORD_CLOUD;
  }

  static isNodeMarkdown(selectedTarget: HTMLElement | null): boolean {
    return selectedTarget?.nodeName?.toLowerCase() === SlotType.MARKDOWN;
  }

  static isSlotTypeEditable(type: SlotType): boolean {
    return type !== SlotType.IMG && type !== SlotType.SOCIAL && type !== SlotType.DEMO;
  }

  static isNodeDragDropResize(selectedTarget: HTMLElement | null): boolean {
    return selectedTarget?.nodeName?.toLowerCase() === SlotType.DRAG_RESIZE_ROTATE;
  }

  static isNodeEditable(element?: HTMLElement): boolean {
    return (
      element?.nodeName?.toLowerCase() === SlotType.CODE ||
      element?.nodeName?.toLowerCase() === SlotType.MATH ||
      element?.nodeName?.toLowerCase() === SlotType.WORD_CLOUD ||
      element?.nodeName?.toLowerCase() === SlotType.MARKDOWN
    );
  }

  static isNodeTitle(selectedTarget: HTMLElement): boolean {
    return [SlotType.H1.toString(), SlotType.H2.toString(), SlotType.H3.toString()].includes(selectedTarget.nodeName.toLowerCase());
  }
}
