import {DeckdeckgoDeckUtils} from '../utils/deckdeckgo-deck-utils';
import {DeckdeckgoExtraUtils} from '../extra/deckdeckgo-extra';

export interface DeckdeckgoSlide {
  beforeSwipe(enter: boolean, _reveal: boolean): Promise<boolean>;

  afterSwipe(): Promise<void>;

  lazyLoadContent(): Promise<void>;

  revealContent(): Promise<void>;

  hideContent(): Promise<void>;
}

export class DeckdeckgoSlideUtils {

  private static showRevealElement(el: HTMLElement): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const elements: NodeListOf<HTMLElement> = el.querySelectorAll('deckgo-reveal, deckgo-reveal-list');

      let couldSwipe: boolean = true;

      if (elements) {
        const nextElement: HTMLElement = Array.from(elements).find((element: HTMLElement) => {
          return !(element as HTMLDeckgoRevealElement | HTMLDeckgoRevealListElement).allElementsRevealed;
        });

        if (nextElement) {
          await (nextElement as HTMLDeckgoRevealElement | HTMLDeckgoRevealListElement).reveal();
          couldSwipe = false;
        }
      }

      resolve(couldSwipe);
    });
  }

  private static hideRevealElement(el: HTMLElement): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      const elements: NodeListOf<HTMLElement> = el.querySelectorAll('deckgo-reveal, deckgo-reveal-list');

      let couldSwipe: boolean = true;

      if (elements) {
        const nextElement: HTMLElement = Array.from(elements).reverse().find((element: HTMLElement) => {
          return !(element as HTMLDeckgoRevealElement | HTMLDeckgoRevealListElement).allElementsHidden;
        });

        if (nextElement) {
          await (nextElement as HTMLDeckgoRevealElement | HTMLDeckgoRevealListElement).hide();
          couldSwipe = false;
        }
      }

      resolve(couldSwipe);
    });
  }

  static showAllRevealElements(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const elements: NodeListOf<HTMLElement> = el.querySelectorAll('deckgo-reveal, deckgo-reveal-list');

      if (elements && elements.length > 0) {
        const promises = [];

        for (const element of  Array.from(elements)) {
          promises.push((element as any).revealAll());
        }

        await Promise.all(promises);
      }

      resolve();
    });
  }

  static hideAllRevealElements(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const elements: NodeListOf<HTMLElement> = el.querySelectorAll('deckgo-reveal, deckgo-reveal-list');

      if (elements && elements.length > 0) {
        const promises = [];

        for (const element of Array.from(elements)) {
          promises.push((element as any).hideAll());
        }

        await Promise.all(promises);
      }

      resolve();
    });
  }

  static beforeSwipe(el: HTMLElement, enter: boolean, reveal: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      if (reveal) {
        const couldSwipe: boolean = enter ? await this.showRevealElement(el) : await this.hideRevealElement(el);
        resolve(couldSwipe);
      } else {
        resolve(true);
      }
    });
  }

  static afterSwipe(): Promise<void> {
    return new Promise<void>((resolve) => {
      resolve();
    });
  };

  static lazyLoadContent(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const promises = [];

      promises.push(DeckdeckgoDeckUtils.lazyLoadImages(el));
      promises.push(DeckdeckgoExtraUtils.lazyLoadContent(el, 'deckgo-gif'));
      promises.push(DeckdeckgoExtraUtils.lazyLoadContent(el, 'deckgo-youtube'));

      await Promise.all(promises);

      resolve();
    });
  }

}
