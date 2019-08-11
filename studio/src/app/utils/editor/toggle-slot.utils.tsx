import {SlotType} from './create-slides.utils';

export class ToggleSlotUtils {

    static toggleSlotType(selectedElement: HTMLElement, type: SlotType): Promise<HTMLElement> {
        return new Promise<HTMLElement>(async (resolve) => {
            if (!selectedElement || !selectedElement.parentElement) {
                resolve(null);
                return;
            }

            if (!document) {
                resolve(null);
                return;
            }

            const element: HTMLElement = document.createElement(type.toString());

            await this.copyAttributes(selectedElement, element);
            await this.cleanAttributes(element, type);
            await this.updateContentEditable(element, type);

            const currentContainer: HTMLElement = this.getSlotContainer(selectedElement);

            const container: HTMLElement = this.createSlotContainer(element, type);

            if (currentContainer.childNodes && currentContainer.childNodes.length > 0) {
                const elements: HTMLElement[] = Array.prototype.slice.call(currentContainer.childNodes);

                elements.forEach((e: HTMLElement) => {
                    container.appendChild(e);
                });
            }

            resolve(element);
        });
    }

    static copyAttributes(selectedElement: HTMLElement, element: HTMLElement): Promise<void> {
        return new Promise<void>((resolve) => {
            if (selectedElement.attributes && selectedElement.attributes.length) {
                for (let i: number = 0; i < selectedElement.attributes.length; i++) {
                    element.setAttribute(selectedElement.attributes[i].name, selectedElement.attributes[i].value);
                }
            }

            resolve();
        });
    }

    private static createSlotContainer(element: HTMLElement, type: SlotType): HTMLElement {
        if (type !== SlotType.CODE) {
            return element;
        }

        const code: HTMLElement = document.createElement('code');
        code.setAttribute('slot', 'code');

        element.appendChild(code);

        return code;
    }

    private static getSlotContainer(selectedElement: HTMLElement): HTMLElement {
        if (selectedElement.firstChild && selectedElement.firstChild instanceof HTMLElement && selectedElement.firstChild.nodeName && selectedElement.firstChild.nodeName.toLowerCase() === 'code') {
            return selectedElement.firstChild;
        } else {
            return selectedElement;
        }
    }

    private static updateContentEditable(selectedElement: HTMLElement, type: SlotType): Promise<void> {
        return new Promise<void>((resolve) => {
            if (type === SlotType.IMG || type === SlotType.SOCIAL) {
                selectedElement.removeAttribute('editable');
                selectedElement.removeAttribute('contenteditable');
            } else if (type === SlotType.CODE) {
                selectedElement.setAttribute('editable', 'true');
                selectedElement.removeAttribute('contenteditable');
            } else {
                selectedElement.setAttribute('contenteditable', 'true');
                selectedElement.removeAttribute('editable');
            }

            resolve();
        });
    }

    private static cleanAttributes(selectedElement: HTMLElement, type: SlotType): Promise<void> {
        return new Promise<void>((resolve) => {
            if (type!== SlotType.IMG && type !== SlotType.SOCIAL) {
                selectedElement.removeAttribute('img-src');
                selectedElement.removeAttribute('img-alt');
                selectedElement.style.removeProperty('justify-content');
                selectedElement.style.removeProperty('--deckgo-lazy-img-width');
            }

            resolve();
        });
    }
}
