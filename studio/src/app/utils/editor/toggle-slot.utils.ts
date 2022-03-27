import {isNodeImage, isNodeList, isNodeReveal, isNodeRevealList, isNodeSocial, isSlotTypeEditable, SlotType} from '@deckdeckgo/studio';
import {RevealSlotUtils} from './reveal-slot.utils';

export class ToggleSlotUtils {
  static toggleSlotType(selectedTarget: HTMLElement, type: SlotType): Promise<HTMLElement> {
    return new Promise<HTMLElement>(async (resolve) => {
      if (!selectedTarget || !selectedTarget.parentElement) {
        resolve(null);
        return;
      }

      if (!document) {
        resolve(null);
        return;
      }

      const element: HTMLElement = document.createElement(type.toString());

      const reveal: boolean = isNodeReveal(selectedTarget);

      await this.copyAttributes(selectedTarget, element);
      await this.cleanAttributes(element, type);
      await this.updateContentEditable(element, type);
      await this.updateLazyImage(element, type);

      await this.copyContent(selectedTarget, element, type, reveal);

      if (reveal) {
        const revealElement: HTMLElement = await RevealSlotUtils.toggleReveal(element, true);
        resolve(revealElement);
      } else {
        resolve(element);
      }
    });
  }

  private static copyAttributes(selectedTarget: HTMLElement, element: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
      if (selectedTarget.attributes && selectedTarget.attributes.length) {
        for (let i: number = 0; i < selectedTarget.attributes.length; i++) {
          element.setAttribute(selectedTarget.attributes[i].name, selectedTarget.attributes[i].value);
        }
      }

      resolve();
    });
  }

  private static async createSlotContainer(element: HTMLElement, type: SlotType): Promise<HTMLElement> {
    if (type == SlotType.CODE) {
      return this.createSlot(element, 'code');
    } else if (type == SlotType.MATH) {
      return this.createSlot(element, 'math');
    } else if (type == SlotType.WORD_CLOUD) {
      return this.createSlot(element, 'words');
    } else if (type == SlotType.MARKDOWN) {
      return this.createSlot(element, 'markdown', 'div');
    } else {
      return element;
    }
  }

  private static async createSlot(
    element: HTMLElement,
    slotName: 'code' | 'math' | 'words' | 'markdown',
    tagName: 'div' | 'code' = 'code'
  ): Promise<HTMLElement> {
    const container: HTMLElement = document.createElement(tagName);
    container.setAttribute('slot', slotName);
    element.appendChild(container);
    return container;
  }

  private static getSlotContainer(selectedTarget: HTMLElement): HTMLElement {
    if (
      selectedTarget.firstChild &&
      selectedTarget.firstChild instanceof HTMLElement &&
      selectedTarget.firstChild.nodeName &&
      (selectedTarget.firstChild.nodeName.toLowerCase() === 'code' || selectedTarget.firstChild.nodeName.toLowerCase() === 'div')
    ) {
      return selectedTarget.firstChild;
    } else {
      return selectedTarget;
    }
  }

  private static updateContentEditable(selectedTarget: HTMLElement, type: SlotType): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!isSlotTypeEditable(type)) {
        selectedTarget.removeAttribute('editable');
        selectedTarget.removeAttribute('contenteditable');
      } else if (type === SlotType.CODE || type == SlotType.MATH || type == SlotType.WORD_CLOUD || type === SlotType.MARKDOWN) {
        selectedTarget.setAttribute('editable', 'true');
        selectedTarget.removeAttribute('contenteditable');
      } else {
        selectedTarget.setAttribute('contenteditable', 'true');
        selectedTarget.removeAttribute('editable');
      }

      resolve();
    });
  }

  private static cleanAttributes(selectedTarget: HTMLElement, type: SlotType): Promise<void> {
    return new Promise<void>((resolve) => {
      if (isSlotTypeEditable(type)) {
        selectedTarget.removeAttribute('img-src');
        selectedTarget.removeAttribute('img-alt');
        selectedTarget.style.removeProperty('justify-content');
        selectedTarget.style.removeProperty('--deckgo-lazy-img-width');
      }

      resolve();
    });
  }

  private static copyContent(selectedTarget: HTMLElement, element: HTMLElement, type: SlotType, reveal: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const currentContainer: HTMLElement = this.getSlotContainer(
        reveal && !isNodeRevealList(selectedTarget) ? (selectedTarget.firstElementChild as HTMLElement) : selectedTarget
      );

      const container: HTMLElement = await this.createSlotContainer(element, type);

      // We don't copy content if the source or the destination is an image
      if (isNodeImage(currentContainer) || isNodeSocial(currentContainer) || !isSlotTypeEditable(type)) {
        resolve();
        return;
      }

      if (type === SlotType.OL || type === SlotType.UL) {
        if (isNodeList(currentContainer)) {
          await this.copyContentChildren(container, currentContainer);
        } else {
          await this.copyContentToList(container, currentContainer);
        }

        resolve();
        return;
      }

      if (isNodeList(currentContainer)) {
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

  private static updateLazyImage(selectedTarget: HTMLElement, type: SlotType): Promise<void> {
    return new Promise<void>((resolve) => {
      if (type === SlotType.IMG) {
        (selectedTarget as HTMLDeckgoLazyImgElement).customLoader = true;
      }

      resolve();
    });
  }
}
