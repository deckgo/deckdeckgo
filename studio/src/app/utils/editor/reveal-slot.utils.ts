import {SlotType, SlotUtils} from '@deckdeckgo/studio';

export class RevealSlotUtils {
  static toggleReveal(selectedTarget: HTMLElement, reveal: boolean): Promise<HTMLElement> {
    return new Promise<HTMLElement>(async (resolve) => {
      if (!selectedTarget) {
        resolve(null);
        return;
      }

      if (!document) {
        resolve(null);
        return;
      }

      const sameReveal: boolean = SlotUtils.isNodeReveal(selectedTarget) === reveal;
      const sameRevealList: boolean = SlotUtils.isNodeRevealList(selectedTarget) === reveal;

      if ((sameReveal && sameRevealList) || (sameReveal && sameRevealList)) {
        resolve(null);
        return;
      }

      let element: HTMLElement;

      if (SlotUtils.isNodeList(selectedTarget)) {
        element = await this.toggleRevealList(reveal, selectedTarget);
      } else {
        element = await this.toggleRevealElement(reveal, selectedTarget);
      }

      if (reveal) {
        (element as any).revealAll();
      }

      resolve(element);
    });
  }

  private static toggleRevealElement(reveal: boolean, selectedTarget: HTMLElement): Promise<HTMLElement> {
    return new Promise<HTMLElement>((resolve) => {
      const element: HTMLElement = reveal ? document.createElement(SlotType.REVEAL) : (selectedTarget.firstElementChild as HTMLElement);

      this.moveSpecificAttributes(selectedTarget, element);

      if (reveal) {
        element.appendChild(selectedTarget.cloneNode(true));
      }

      // For styling purpose, we need to identify reveal element with images
      if (SlotUtils.isNodeImage(selectedTarget)) {
        element.setAttribute('img', '');
      } else {
        element.removeAttribute('img');
      }

      resolve(element);
    });
  }

  private static toggleRevealList(reveal: boolean, selectedTarget: HTMLElement): Promise<HTMLElement> {
    return new Promise<HTMLElement>((resolve) => {
      let element: HTMLElement;

      if (reveal) {
        element = document.createElement(SlotType.REVEAL_LIST);

        if (selectedTarget?.nodeName?.toLowerCase() === SlotType.UL) {
          element.setAttribute('list-tag', SlotType.UL);
        }
      } else {
        element =
          selectedTarget.getAttribute('list-tag') === SlotType.UL
            ? document.createElement(SlotType.UL)
            : document.createElement(SlotType.OL);
      }

      // In case of deckgo-reveal-list the contenteditable should be set on the host not the slot
      element.setAttribute('contenteditable', '');

      this.moveSpecificAttributes(selectedTarget, element);

      element.append(...Array.from(selectedTarget.children));

      resolve(element);
    });
  }

  private static moveSpecificAttributes(selectedTarget: HTMLElement, element: HTMLElement) {
    if (selectedTarget.hasAttribute('slot')) {
      element.setAttribute('slot', selectedTarget.getAttribute('slot'));
      selectedTarget.removeAttribute('slot');
    }

    if (selectedTarget.hasAttribute('style')) {
      element.setAttribute('style', selectedTarget.getAttribute('style'));
      selectedTarget.removeAttribute('style');
    }

    if (selectedTarget.hasAttribute('highlighted')) {
      selectedTarget.removeAttribute('highlighted');
    }
  }
}
