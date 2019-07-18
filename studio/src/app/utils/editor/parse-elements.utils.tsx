import {h} from '@stencil/core';

import {ParseStyleUtils} from './parse-style.utils';
import {SlotType} from './create-slides.utils';

export class ParseElementsUtils {

    static parseElements(element: HTMLElement, root: boolean): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!element) {
                resolve(undefined);
                return;
            }

            if (element.nodeType === 3) {
                resolve(element.textContent + '\n');
                return;
            }

            if (element.hasChildNodes()) {
                const results = [];

                const elements: HTMLElement[] = Array.prototype.slice.call(element.childNodes);

                for (const elem of elements) {
                    const result = await this.parseElements(elem, false);
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
            const Elem: string = element.nodeName;

            const attributes: any = this.getAttributes(element);
            if (attributes.style) {
                attributes.style = await ParseStyleUtils.convertStyle(attributes.style);
            }

            if (this.isElementContentEditable(element, attributes)) {
                attributes['contenteditable'] = true;
            }

            if (element.nodeName && element.nodeName.toLowerCase() === 'deckgo-lazy-img') {
                attributes['contenteditable'] = 'false';
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
                return acc
            }, {});
    }

    private static isElementContentEditable(element: HTMLElement, attributes: any): boolean {
        return attributes.slot &&
            attributes.slot !== 'background' &&
            (!element.nodeName || (element.nodeName.toLowerCase() !== 'code' && element.nodeName.toLowerCase() !== SlotType.CODE)) &&
            (!element.nodeName || (element.nodeName.toLowerCase() !== 'deckgo-social' && element.nodeName.toLowerCase() !== SlotType.SOCIAL));
    }

}
