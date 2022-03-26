import {SlotType} from '../types/slot-type';

export const isNodeReveal = (selectedTarget: HTMLElement): boolean => selectedTarget?.nodeName?.toLowerCase() === SlotType.REVEAL;

export const isNodeRevealList = (selectedTarget: HTMLElement): boolean => selectedTarget?.nodeName?.toLowerCase() === SlotType.REVEAL_LIST;

export const isNodeList = (selectedTarget: HTMLElement): boolean =>
  selectedTarget &&
  selectedTarget.nodeName &&
  (selectedTarget.nodeName.toLowerCase() === SlotType.OL ||
    selectedTarget.nodeName.toLowerCase() === SlotType.UL ||
    selectedTarget.nodeName.toLowerCase() === SlotType.REVEAL_LIST);

export const isNodeImage = (selectedTarget: HTMLElement | null): boolean => selectedTarget?.nodeName?.toLowerCase() === SlotType.IMG;

export const isNodeSocial = (selectedTarget: HTMLElement | null): boolean => selectedTarget?.nodeName?.toLowerCase() === SlotType.SOCIAL;

export const isNodeCode = (selectedTarget: HTMLElement | null): boolean => selectedTarget?.nodeName?.toLowerCase() === SlotType.CODE;

export const isNodeWordCloud = (selectedTarget: HTMLElement | null): boolean =>
  selectedTarget?.nodeName?.toLowerCase() === SlotType.WORD_CLOUD;

export const isNodeMarkdown = (selectedTarget: HTMLElement | null): boolean =>
  selectedTarget?.nodeName?.toLowerCase() === SlotType.MARKDOWN;

export const isSlotTypeEditable = (type: SlotType): boolean => type !== SlotType.IMG && type !== SlotType.SOCIAL && type !== SlotType.DEMO;

export const isNodeDragDropResize = (selectedTarget: HTMLElement | null): boolean =>
  selectedTarget?.nodeName?.toLowerCase() === SlotType.DRAG_RESIZE_ROTATE;

export const isNodeEditable = (element?: HTMLElement): boolean =>
  element?.nodeName?.toLowerCase() === SlotType.CODE ||
  element?.nodeName?.toLowerCase() === SlotType.MATH ||
  element?.nodeName?.toLowerCase() === SlotType.WORD_CLOUD ||
  element?.nodeName?.toLowerCase() === SlotType.MARKDOWN;

export const isNodeTitle = (selectedTarget: HTMLElement): boolean =>
  [SlotType.H1.toString(), SlotType.H2.toString(), SlotType.H3.toString()].includes(selectedTarget.nodeName.toLowerCase());
