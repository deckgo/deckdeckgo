export class DeckdeckgoDocsUtils {

  static reloadCode(el: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
      let elements: HTMLElement[] = this.getAllDeckgoHighlightCode(el);

      if (!elements || elements.length <= 0) {
        resolve();
      } else {
        elements.forEach(async (element: HTMLElement) => {

          if (element.hasOwnProperty('load')) {
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

  static loadVideo(el: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
      let elements: HTMLElement[] = this.getAllDeckgoYoutube(el);

      if (!elements || elements.length <= 0) {
        resolve();
      } else {
        elements.forEach(async (element: HTMLElement) => {

          if (element.hasOwnProperty('lazyLoadContent')) {
            await await (element as any).lazyLoadContent();
          }
        });

        resolve();
      }
    });
  }

  private static getAllDeckgoYoutube(el: HTMLElement): HTMLElement[] {
    const allElements: NodeListOf<HTMLElement> = el.querySelectorAll('deckgo-youtube');

    if (el.shadowRoot) {
      const allShadowedElements: NodeListOf<HTMLElement> = el.shadowRoot.querySelectorAll('deckgo-youtube');
      return Array.from(allElements).concat(Array.from(allShadowedElements));
    } else {
      return Array.from(allElements);
    }
  }

}
