import {SlotType} from '../types/slot-type';
import {AttrType} from './parse-elements.utils';

export const isContentEditableDeck = (element: HTMLElement, attributes: AttrType): boolean => {
  return attributes.slot !== undefined && attributes.slot !== 'background' && isElementContentEditable(element);
};

export const isElementContentEditable = (element: HTMLElement): boolean => {
  return (
    (!element.nodeName || (element.nodeName.toLowerCase() !== 'code' && element.nodeName.toLowerCase() !== SlotType.CODE)) &&
    (!element.nodeName || (element.nodeName.toLowerCase() !== 'code' && element.nodeName.toLowerCase() !== SlotType.MATH)) &&
    (!element.nodeName || (element.nodeName.toLowerCase() !== 'code' && element.nodeName.toLowerCase() !== SlotType.WORD_CLOUD)) &&
    (!element.nodeName || (element.nodeName.toLowerCase() !== 'div' && element.nodeName.toLowerCase() !== SlotType.MARKDOWN)) &&
    (!element.nodeName || (element.nodeName.toLowerCase() !== 'deckgo-social' && element.nodeName.toLowerCase() !== SlotType.SOCIAL)) &&
    (!element.nodeName || (element.nodeName.toLowerCase() !== 'deckgo-lazy-img' && element.nodeName.toLowerCase() !== SlotType.IMG)) &&
    (!element.nodeName || (element.nodeName.toLowerCase() !== 'deckgo-demo' && element.nodeName.toLowerCase() !== SlotType.DEMO)) &&
    (!element.nodeName || (element.nodeName.toLowerCase() !== 'deckgo-reveal' && element.nodeName.toLowerCase() !== SlotType.REVEAL)) &&
    (!element.nodeName ||
      (element.nodeName.toLowerCase() !== 'deckgo-drr' && element.nodeName.toLowerCase() !== SlotType.DRAG_RESIZE_ROTATE)) &&
    isElementPollSlotEditable(element)
  );
};

const isElementPollSlotEditable = (element: HTMLElement): boolean => {
  return !element.hasAttribute('slot') || (element.getAttribute('slot') !== 'awaiting-votes' && element.getAttribute('slot') !== 'how-to');
};
