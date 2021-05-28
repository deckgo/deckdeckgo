import {h} from '@stencil/core';

import {convertStyle} from '@deckdeckgo/deck-utils';

export class ParseElementsUtils {
  static parseElements(element: HTMLElement, root: boolean, codeChild: boolean = false): Promise<any> {
    return new Promise<any>(async (resolve) => {
      if (!element) {
        resolve(undefined);
        return;
      }

      if (element.nodeType === 3) {
        resolve(`${element.textContent}${!codeChild ? '\n' : ''}`);
        return;
      }

      if (element.hasChildNodes()) {
        const results = [];

        const elements: HTMLElement[] = Array.prototype.slice.call(element.childNodes);

        for (const elem of elements) {
          const result = await this.parseElements(elem, false, codeChild || element.nodeName.toLowerCase() === 'code');
          results.push(result);
        }

        resolve(root ? results : await this.parseElement(element, results));
      } else {
        resolve(await this.parseElement(element, element.textContent));
      }
    });
  }

  private static parseElement(element: HTMLElement, content: any): Promise<any> {
    return new Promise<any>(async (resolve) => {
      const Elem: string = element.nodeName.toLowerCase();

      const attributes: any = this.getAttributes(element);
      if (attributes.style) {
        attributes.style = await convertStyle(attributes.style);
      }

      resolve(<Elem {...attributes}>{content}</Elem>);
    });
  }

  private static getAttributes(el): any {
    if (!el || !el.attributes) {
      return {};
    }

    return Array.from(el.attributes)
      .map((a: Attr) => [a.name, a.value])
      .reduce((acc, attr) => {
        acc[attr[0]] = attr[1];
        return acc;
      }, {});
  }
}
