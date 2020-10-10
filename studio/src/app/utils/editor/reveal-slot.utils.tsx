import {SlotUtils} from './slot.utils';
import {SlotType} from './slot-type';

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

      const sameReveal: boolean = SlotUtils.isNodeReveal(selectedElement) === reveal;
      const sameRevealList: boolean = SlotUtils.isNodeRevealList(selectedElement) === reveal;

      if ((sameReveal && sameRevealList) || (sameReveal && sameRevealList)) {
        resolve(null);
        return;
      }

      let element: HTMLElement;

      if (SlotUtils.isNodeList(selectedElement)) {
        element = await this.toggleRevealList(reveal, selectedElement);
      } else {
        element = await this.toggleRevealElement(reveal, selectedElement);
      }

      if (reveal) {
        (element as any).revealAll();
      }

      resolve(element);
    });
  }

  private static toggleRevealElement(reveal: boolean, selectedElement: HTMLElement): Promise<HTMLElement> {
    return new Promise<HTMLElement>((resolve) => {
      const element: HTMLElement = reveal ? document.createElement(SlotType.REVEAL) : (selectedElement.firstElementChild as HTMLElement);

      this.moveSpecificAttributes(selectedElement, element);

      if (reveal) {
        element.appendChild(selectedElement.cloneNode(true));
      }

      // For styling purpose, we need to identify reveal element with images
      if (SlotUtils.isNodeImage(selectedElement)) {
        element.setAttribute('img', '');
      } else {
        element.removeAttribute('img');
      }

      resolve(element);
    });
  }

  private static toggleRevealList(reveal: boolean, selectedElement: HTMLElement): Promise<HTMLElement> {
    return new Promise<HTMLElement>((resolve) => {
      let element: HTMLElement;

      if (reveal) {
        element = document.createElement(SlotType.REVEAL_LIST);

        if (selectedElement.nodeName?.toLowerCase() === SlotType.UL) {
          element.setAttribute('list-tag', SlotType.UL);
        }
      } else {
        element = selectedElement.getAttribute('list-tag') === SlotType.UL ? document.createElement(SlotType.UL) : document.createElement(SlotType.OL);
      }

      // In case of deckgo-reveal-list the contenteditable should be set on the host not the slot
      element.setAttribute('contenteditable', '');

      this.moveSpecificAttributes(selectedElement, element);

      element.append(...Array.from(selectedElement.children));

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

    if (selectedElement.hasAttribute('highlighted')) {
      selectedElement.removeAttribute('highlighted');
    }
  }
}
