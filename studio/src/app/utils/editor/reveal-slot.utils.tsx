import {SlotType} from './create-slides.utils';

export class RevealSlotUtils {

    static toggleReveal(selectedElement: HTMLElement, reveal: boolean): Promise<HTMLElement> {
        return new Promise<HTMLElement>(async (resolve) => {
            if (!selectedElement) {
                resolve(null);
                return;
            }

            if (!document) {
                resolve(null);
                return;
            }

            if (this.isNodeReveal(selectedElement) === reveal) {
                resolve(null);
                return;
            }

            const element: HTMLElement = reveal ? document.createElement(SlotType.REVEAL) : selectedElement.firstElementChild as HTMLElement;

            this.moveSpecificAttributes(selectedElement, element);

            if (reveal) {
                element.appendChild(selectedElement.cloneNode(true));

                (element as any).reveal();
            }

            resolve(element);

        });
    }

    private static moveSpecificAttributes(selectedElement: HTMLElement, element: HTMLElement) {
        if (selectedElement.hasAttribute('slot')) {
            element.setAttribute('slot', selectedElement.getAttribute('slot'));
            selectedElement.removeAttribute('slot');
        }

        if (selectedElement.hasAttribute('style')) {
            element.setAttribute('style', selectedElement.getAttribute('style'));
            selectedElement.removeAttribute('style');
        }

        if (selectedElement.hasAttribute('contenteditable')) {
            element.setAttribute('contenteditable', selectedElement.getAttribute('contenteditable'));
        }
    }

    static isNodeReveal(selectedElement: HTMLElement): boolean {
        return selectedElement && selectedElement.nodeName && selectedElement.nodeName.toLowerCase() === SlotType.REVEAL;
    }
}
