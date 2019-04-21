import {SlotType} from './create-slides.utils';

export class ToggleSlotUtils {

    static toggleSlotType(selectedElement: HTMLElement, type: SlotType): Promise<HTMLElement> {
        return new Promise<HTMLElement>(async (resolve) => {
            if (!selectedElement || !selectedElement.parentElement) {
                resolve();
                return;
            }
            
            if (!document) {
                resolve();
                return;
            }

            const element: HTMLElement = document.createElement(type.toString());

            if (selectedElement.attributes && selectedElement.attributes.length) {
                for (let i: number = 0; i < selectedElement.attributes.length; i++) {
                    element.setAttribute(selectedElement.attributes[i].name, selectedElement.attributes[i].value);
                }
            }

            const currentContainer: HTMLElement = this.getSlotContainer(selectedElement);

            if (currentContainer.childNodes && currentContainer.childNodes.length > 0) {
                const elements: HTMLElement[] = Array.prototype.slice.call(currentContainer.childNodes);

                const container: HTMLElement = this.createSlotContainer(element, type);

                elements.forEach((e: HTMLElement) => {
                    container.appendChild(e);
                });
            }
            
            resolve(element);
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

    private static getSlotContainer(selectedElement: HTMLElement):HTMLElement {
        if (selectedElement.firstChild && selectedElement.firstChild instanceof HTMLElement && selectedElement.firstChild.nodeName && selectedElement.firstChild.nodeName.toLowerCase() === 'code') {
            return selectedElement.firstChild;
        } else {
            return selectedElement;
        }
    }
}
