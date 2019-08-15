import {h} from '@stencil/core';

import {ParseStyleUtils} from './parse-style.utils';
import {SlotType} from './slot-type';
import {SlotUtils} from './slot.utils';

export class ParseElementsUtils {

    static parseElements(element: HTMLElement, root: boolean, contentEditable: boolean): Promise<any> {
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
            const Elem: string = element.nodeName;

            const attributes: any = this.getAttributes(element);
            if (attributes.style) {
                attributes.style = await ParseStyleUtils.convertStyle(attributes.style);
            }

            if (contentEditable && this.isContentEditable(element, attributes)) {
                attributes['contenteditable'] = true;
            } else if (contentEditable && SlotUtils.isNodeReveal(element) && element.firstElementChild) {
                element.firstElementChild.setAttribute('contenteditable', `${true}`);
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

    private static isContentEditable(element: HTMLElement, attributes: any): boolean {
        return attributes.slot &&
            attributes.slot !== 'background' &&
            this.isElementContentEditable(element);
    }

    static isElementContentEditable(element: HTMLElement): boolean {
        return (!element.nodeName || (element.nodeName.toLowerCase() !== 'code' && element.nodeName.toLowerCase() !== SlotType.CODE)) &&
            (!element.nodeName || (element.nodeName.toLowerCase() !== 'deckgo-social' && element.nodeName.toLowerCase() !== SlotType.SOCIAL)) &&
            (!element.nodeName || (element.nodeName.toLowerCase() !== 'deckgo-lazy-img' && element.nodeName.toLowerCase() !== SlotType.IMG)) &&
            (!element.nodeName || (element.nodeName.toLowerCase() !== 'deckgo-reveal' && element.nodeName.toLowerCase() !== SlotType.REVEAL));
    }
}
