import {RevealSlotUtils} from './reveal-slot.utils';
import {SlotType} from './slot-type';
import {SlotUtils} from './slot.utils';

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

            const reveal: boolean = SlotUtils.isNodeReveal(selectedElement);

            await this.copyAttributes(selectedElement, element);
            await this.cleanAttributes(element, type);
            await this.updateContentEditable(element, type);

            await this.copyContent(selectedElement, element, type, reveal);

            if (reveal) {
                const revealElement: HTMLElement = await RevealSlotUtils.toggleReveal(element, true);
                resolve(revealElement);
            } else {
                resolve(element);
            }
        });
    }

    private static copyAttributes(selectedElement: HTMLElement, element: HTMLElement): Promise<void> {
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
            if (type !== SlotType.IMG && type !== SlotType.SOCIAL) {
                selectedElement.removeAttribute('img-src');
                selectedElement.removeAttribute('img-alt');
                selectedElement.style.removeProperty('justify-content');
                selectedElement.style.removeProperty('--deckgo-lazy-img-width');
            }

            resolve();
        });
    }

    private static copyContent(selectedElement: HTMLElement, element: HTMLElement, type: SlotType, reveal: boolean): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const currentContainer: HTMLElement = this.getSlotContainer(reveal && !SlotUtils.isNodeRevealList(selectedElement) ? selectedElement.firstElementChild as HTMLElement : selectedElement);

            // We don't copy content if the source or the destination is an image
            if ((SlotUtils.isNodeImage(currentContainer) || SlotUtils.isNodeSocial(currentContainer)) ||
                (type === SlotType.IMG || type === SlotType.SOCIAL)) {
                resolve();
                return;
            }

            const container: HTMLElement = this.createSlotContainer(element, type);

            if (type === SlotType.OL || type === SlotType.UL) {
                if (SlotUtils.isNodeList(currentContainer)) {
                    await this.copyContentChildren(container, currentContainer);
                } else {
                    await this.copyContentToList(container, currentContainer);
                }

                resolve();
                return;
            }

            if (SlotUtils.isNodeList(currentContainer)) {
                await this.copyContentFromList(container, currentContainer);

                resolve();
                return;
            }

            await this.copyContentChildren(container, currentContainer);

            resolve();
        });
    }

    private static copyContentToList(container: HTMLElement, currentContainer: HTMLElement): Promise<void> {
        return new Promise<void>((resolve) => {
            const element: HTMLElement = document.createElement('li');

            if (currentContainer.innerText && currentContainer.innerText !== undefined && currentContainer.innerText !== '') {
                element.innerHTML = currentContainer.innerText;
            } else {
                // If no spacer is added, then on focus the cursor will be placed on the parent and not on the li
                // Trade of, because of that, the css:empty will not work
                const zeroWidthSpacer: Text = document.createTextNode('\u200B');
                element.appendChild(zeroWidthSpacer);
            }

            container.appendChild(element);

            resolve();
        });
    }

    private static copyContentFromList(container: HTMLElement, currentContainer: HTMLElement): Promise<void> {
        return new Promise<void>((resolve) => {
            container.innerText = currentContainer.innerText;

            resolve();
        });
    }

    private static copyContentChildren(container: HTMLElement, currentContainer: HTMLElement): Promise<void> {
        return new Promise<void>((resolve) => {
            if (currentContainer.childNodes && currentContainer.childNodes.length > 0) {
                const elements: HTMLElement[] = Array.prototype.slice.call(currentContainer.childNodes);

                elements.forEach((e: HTMLElement) => {
                    container.appendChild(e);
                });
            }

            resolve();
        });
    }

}
