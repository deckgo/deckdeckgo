import {ToggleSlotUtils} from './toggle-slot.utils';

export class RevealSlotUtils {

    static toggleReveal(selectedElement: HTMLElement, reveal: boolean): Promise<HTMLElement> {
        return new Promise<HTMLElement>(async (resolve) => {
            if (!selectedElement || !selectedElement.parentElement) {
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

            const element: HTMLElement = reveal ? document.createElement('deckgo-reveal') : selectedElement.firstElementChild as HTMLElement;
            await ToggleSlotUtils.copyAttributes(selectedElement, element);

            if (reveal) {
                const attributeNames: string[] = selectedElement.getAttributeNames();
                if (attributeNames && attributeNames.length > 0) {
                    for (const attributeName of attributeNames) {
                        if (attributeName && attributeName.toLowerCase() !== 'contenteditable') {
                            selectedElement.removeAttribute(attributeName);
                        }
                    }
                }
                element.appendChild(selectedElement.cloneNode(true));

                (element as any).reveal();
            }

            resolve(element);

        });
    }

    static isNodeReveal(selectedElement: HTMLElement): boolean {
        return selectedElement && selectedElement.nodeName && selectedElement.nodeName.toLowerCase() === 'deckgo-reveal';
    }
}
