import {DeckdeckgoUtils} from '../utils/deckdeckgo-utils';
import {DeckdeckgoExtraUtils} from '../extra/deckdeckgo-extra';

export interface DeckdeckgoSlide {
  beforeSwipe(_enter: boolean): Promise<boolean>;

  afterSwipe(): Promise<void>;

  lazyLoadContent(): Promise<void>;
}

export class DeckdeckgoSlideUtils {

  static hideRevealElements(el: HTMLElement, revealShowFirst: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      // No keyboard on mobile device to reveal elements
      if (DeckdeckgoUtils.isMobile()) {
        resolve();
        return;
      }

      const elements: NodeListOf<HTMLElement> = el.querySelectorAll(revealShowFirst ? '[slot] li:not(:first-child), [slot] > p:not(:first-child), [slot] > span:not(:first-child), [slot] > img:not(:first-child)' : '[slot] li, [slot] > p, [slot] > span, [slot] > img');

      if (!elements) {
        resolve();
      } else {
        Array.from(elements).forEach((element: HTMLElement) => {
          element.style.setProperty('visibility', 'hidden');
          element.style.setProperty('transform', 'var(--slide-reveal-transform)');
          element.style.setProperty('opacity', '0');

          element.classList.add('deckgo-reveal');
          element.style.setProperty('transition', 'all var(--slide-reveal-duration) cubic-bezier(0.23, 1, 0.320, 1)');

        });
      }
    });
  }

  private static showRevealElement(el: HTMLElement): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      const elements: NodeListOf<HTMLElement> = el.querySelectorAll('[slot] li, [slot] > p, [slot] > span, [slot] > img');

      let couldSwipe: boolean = true;

      if (elements) {
        const nextElement: HTMLElement = Array.from(elements).find((element: HTMLElement) => {
          return element.style.getPropertyValue('visibility') === 'hidden';
        });

        if (nextElement) {
          nextElement.style.setProperty('visibility', 'initial');
          nextElement.style.setProperty('opacity', '1');
          nextElement.style.setProperty('transform', 'none');
          couldSwipe = false;
        }
      }

      resolve(couldSwipe);
    });
  }

  private static hideRevealElement(el: HTMLElement): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      const elements: NodeListOf<HTMLElement> = el.querySelectorAll('[slot] li, [slot] > p, [slot] > span, [slot] > img');

      let couldSwipe: boolean = true;

      if (elements) {
        const nextElement: HTMLElement = Array.from(elements).reverse().find((element: HTMLElement) => {
          const property: string = element.style.getPropertyValue('visibility');
          return !property || property === 'initial';
        });

        if (nextElement) {
          nextElement.style.setProperty('visibility', 'hidden');
          nextElement.style.setProperty('transform', 'var(--slide-reveal-transform)');
          nextElement.style.setProperty('opacity', '0');
          couldSwipe = false;
        }
      }

      resolve(couldSwipe);
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

      promises.push(DeckdeckgoUtils.lazyLoadImages(el));
      promises.push(DeckdeckgoExtraUtils.lazyLoadContent(el, 'deckgo-gif'));
      promises.push(DeckdeckgoExtraUtils.lazyLoadContent(el, 'deckgo-youtube'));

      await Promise.all(promises);

      resolve();
    });
  }

}
