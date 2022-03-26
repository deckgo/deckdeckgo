import {attributes as getAttributes, convertStyle, isTextNode} from '@deckdeckgo/editor';
import {h, JSX} from '@stencil/core';
import {SlotType} from '../types/slot-type';
import {isNodeReveal} from './slot.utils';

type AttrType = Record<string, string | Record<string, string> | undefined>;

export const parseElements = (element: Node, root: boolean, contentEditable: boolean): JSX.IntrinsicElements | JSX.IntrinsicElements[] | string | undefined => {
  if (!element) {
    return undefined;
  }

  if (isTextNode(element)) {
    return element.textContent;
  }

  if (element.hasChildNodes()) {
    const results = [];

    for (const elem of Array.from(element.childNodes)) {
      const result = parseElements(elem, false, contentEditable);
      results.push(result);
    }

    return root ? results : parseElement(element as HTMLElement, results, contentEditable);
  }

  return parseElement(element as HTMLElement, element.textContent, contentEditable);
};

const parseElement = (element: HTMLElement, content: JSX.IntrinsicElements[] | string, contentEditable: boolean): JSX.IntrinsicElements => {
  const Elem: string = element.nodeName.toLowerCase();

  const attributes: AttrType = getAttributes(element);
  if (attributes.style) {
    attributes.style = convertStyle(attributes.style as string);
  }

  if (contentEditable && isContentEditable(element, attributes)) {
    if (contentEditable && isNodeReveal(element) && element.firstElementChild) {
      element.firstElementChild.setAttribute('contenteditable', `${true}`);
    } else {
      attributes['contenteditable'] = `${true}`;
    }
  }

  if (element && element.nodeName && element.nodeName.toLowerCase() === 'deckgo-lazy-img') {
    attributes['custom-loader'] = `${true}`;
  }

  return <Elem {...attributes}>{content}</Elem>;
};

const isContentEditable = (element: HTMLElement, attributes: AttrType): boolean => {
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
