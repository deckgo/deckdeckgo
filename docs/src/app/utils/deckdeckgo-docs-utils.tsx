export class DeckdeckgoDocsUtils {
  static reloadCode(el: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
      let elements: HTMLElement[] = this.getAllDeckgoHighlightCode(el);

      if (!elements || elements.length <= 0) {
        resolve();
      } else {
        elements.forEach(async (element: HTMLElement) => {
          if (element && 'load' in element) {
            await await (element as any).load();
          }
        });

        resolve();
      }
    });
  }

  private static getAllDeckgoHighlightCode(el: HTMLElement): HTMLElement[] {
    const allElements: NodeListOf<HTMLElement> = el.querySelectorAll('deckgo-highlight-code');

    if (el.shadowRoot) {
      const allShadowedElements: NodeListOf<HTMLElement> = el.shadowRoot.querySelectorAll('deckgo-highlight-code');
      return Array.from(allElements).concat(Array.from(allShadowedElements));
    } else {
      return Array.from(allElements);
    }
  }

  static async lazyLoadElements(el: HTMLElement, selector: string) {
    const elements: HTMLElement[] = this.getAllDeckgoYoutube(el, selector);

    if (!elements || elements.length <= 0) {
      return;
    }

    const filterElements: HTMLElement[] = elements.filter((element: HTMLElement) => {
      return element.hasOwnProperty('lazyLoadContent');
    });

    if (!filterElements || filterElements.length <= 0) {
      return;
    }

    const promises: Promise<void>[] = filterElements.map((element: HTMLElement) => {
      return (element as any).lazyLoadContent();
    });

    await Promise.all(promises);
  }

  private static getAllDeckgoYoutube(el: HTMLElement, selector: string): HTMLElement[] {
    const allElements: NodeListOf<HTMLElement> = el.querySelectorAll(selector);

    if (el.shadowRoot) {
      const allShadowedElements: NodeListOf<HTMLElement> = el.shadowRoot.querySelectorAll(selector);
      return Array.from(allElements).concat(Array.from(allShadowedElements));
    } else {
      return Array.from(allElements);
    }
  }

  static initSlideSize(deck: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!deck) {
        return;
      }

      await (deck as any).initSlideSize();

      resolve();
    });
  }
}
