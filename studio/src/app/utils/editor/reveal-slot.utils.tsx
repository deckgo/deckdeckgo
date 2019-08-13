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

            const list: boolean = this.isNodeList(selectedElement);

            let element: HTMLElement;

            if (list) {
                element = reveal ? document.createElement(SlotType.REVEAL_LIST) : document.createElement(SlotType.OL);

                // In case of deckgo-reveal-list the contenteditable should be set on the host not the slot
                element.setAttribute('contenteditable', '');

                this.moveSpecificAttributes(selectedElement, element);

                element.append(...Array.from(selectedElement.children));
            } else {
                element = reveal ? document.createElement(SlotType.REVEAL) : selectedElement.firstElementChild as HTMLElement;

                this.moveSpecificAttributes(selectedElement, element);

                if (reveal) {
                    element.appendChild(selectedElement.cloneNode(true));
                }
            }

            if (reveal) {
                (element as any).revealAll();
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
    }

    static isNodeReveal(selectedElement: HTMLElement): boolean {
        return selectedElement && selectedElement.nodeName &&
            (selectedElement.nodeName.toLowerCase() === SlotType.REVEAL ||
                selectedElement.nodeName.toLowerCase() === SlotType.REVEAL_LIST);
    }

    static isNodeRevealList(selectedElement: HTMLElement): boolean {
        return selectedElement && selectedElement.nodeName && selectedElement.nodeName.toLowerCase() === SlotType.REVEAL_LIST;
    }

    static isNodeList(selectedElement: HTMLElement): boolean {
        return selectedElement && selectedElement.nodeName &&
            (selectedElement.nodeName.toLowerCase() === SlotType.OL ||
                selectedElement.nodeName.toLowerCase() === SlotType.UL ||
                selectedElement.nodeName.toLowerCase() === SlotType.REVEAL_LIST);
    }
}
