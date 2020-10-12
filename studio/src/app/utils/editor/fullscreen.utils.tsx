export class FullscreenUtils {
  static async toggleEditable(editable: boolean) {
    const deck: HTMLElement = document.querySelector('main > deckgo-deck');

    if (!deck) {
      return false;
    }

    const slots: NodeListOf<HTMLElement> = deck.querySelectorAll('code[slot]');

    if (!slots || slots.length <= 0) {
      return false;
    }

    const promises: Promise<void>[] = Array.from(slots).map((element: HTMLElement) => this.toggleSlot(element, editable));
    await Promise.all(promises);
  }

  private static async toggleSlot(slotElement: HTMLElement, editable: boolean) {
    slotElement.parentElement?.setAttribute('editable', editable ? '' : 'false');
  }
}
