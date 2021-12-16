import {h} from '@stencil/core';

import {convertStyle} from '@deckdeckgo/editor';

import {SlotType} from '../../types/editor/slot-type';

import {SlotUtils} from './slot.utils';
import {getAttributes} from './attributes.utils';

export class ParseElementsUtils {
  static parseElements(element: HTMLElement, root: boolean, contentEditable: boolean): Promise<any> {
    return new Promise<any>(async (resolve) => {
      if (!element) {
        resolve(undefined);
        return;
      }

      if (element.nodeType === 3) {
        resolve(element.textContent);
        return;
      }

      if (element.hasChildNodes()) {
        const results = [];

        const elements: HTMLElement[] = Array.prototype.slice.call(element.childNodes);

        for (const elem of elements) {
          const result = await this.parseElements(elem, false, contentEditable);
          results.push(result);
        }

        resolve(root ? results : await this.parseElement(element, results, contentEditable));
      } else {
        resolve(await this.parseElement(element, element.textContent, contentEditable));
      }
    });
  }

  private static parseElement(element: HTMLElement, content: any, contentEditable: boolean): Promise<any> {
    return new Promise<any>(async (resolve) => {
      const Elem: string = element.nodeName.toLowerCase();

      const attributes: Record<string, string | Record<string, string> | undefined> = getAttributes(element);
      if (attributes.style) {
        attributes.style = convertStyle(attributes.style as string);
      }

      if (contentEditable && this.isContentEditable(element, attributes)) {
        if (contentEditable && SlotUtils.isNodeReveal(element) && element.firstElementChild) {
          element.firstElementChild.setAttribute('contenteditable', `${true}`);
        } else {
          attributes['contenteditable'] = `${true}`;
        }
      }

      if (element && element.nodeName && element.nodeName.toLowerCase() === 'deckgo-lazy-img') {
        attributes['custom-loader'] = `${true}`;
      }

      resolve(<Elem {...attributes}>{content}</Elem>);
    });
  }

  private static isContentEditable(element: HTMLElement, attributes: any): boolean {
    return attributes.slot !== undefined && attributes.slot !== 'background' && this.isElementContentEditable(element);
  }

  static isElementContentEditable(element: HTMLElement): boolean {
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
      this.isElementPollSlotEditable(element)
    );
  }

  private static isElementPollSlotEditable(element: HTMLElement): boolean {
    return (
      !element.hasAttribute('slot') || (element.getAttribute('slot') !== 'awaiting-votes' && element.getAttribute('slot') !== 'how-to')
    );
  }
}
