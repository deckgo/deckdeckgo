export interface DeckdeckgoSlide {
  beforeSwipe(swipeLeft: boolean): Promise<boolean>;

  lazyLoadImages(): Promise<void>;
}

export class DeckDeckGoSlideUtils {

  static hideElements(el: HTMLElement, revealShowFirst: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      const elements: NodeListOf<HTMLElement> = el.querySelectorAll(revealShowFirst ? '[slot] > li:not(:first-child), [slot] > p:not(:first-child), [slot] > img:not(:first-child)' : '[slot] > li, [slot] > p, [slot] > img');

      if (!elements) {
        resolve();
      } else {
        Array.from(elements).forEach((element: HTMLElement) => {
          element.style.setProperty('visibility', 'hidden');
        });
      }
    });
  }

  private static showElement(el: HTMLElement): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      const elements: NodeListOf<HTMLElement> = el.querySelectorAll('[slot] > li, [slot] > p, [slot] > img');

      let couldSwipe: boolean = true;

      if (elements) {
        const nextElement: HTMLElement = Array.from(elements).find((element: HTMLElement) => {
          return element.style.getPropertyValue('visibility') === 'hidden';
        });

        if (nextElement) {
          nextElement.style.setProperty('visibility', 'initial');
          couldSwipe = false;
        }
      }

      resolve(couldSwipe);
    });
  }

  private static hideElement(el: HTMLElement): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      const elements: NodeListOf<HTMLElement> = el.querySelectorAll('[slot] > li, [slot] > p, [slot] > img');

      let couldSwipe: boolean = true;

      if (elements) {
        const nextElement: HTMLElement = Array.from(elements).reverse().find((element: HTMLElement) => {
          const property: string = element.style.getPropertyValue('visibility');
          return !property || property === 'initial';
        });

        if (nextElement) {
          nextElement.style.setProperty('visibility', 'hidden');
          couldSwipe = false;
        }
      }

      resolve(couldSwipe);
    });
  }

  static beforeSwipe(el: HTMLElement, swipeLeft: boolean, reveal: boolean): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      if (reveal) {
        const couldSwipe: boolean = swipeLeft ? await this.showElement(el) : await this.hideElement(el);
        resolve(couldSwipe);
      } else {
        resolve(true);
      }
    });
  }

  static async lazyLoadImages(el: HTMLElement): Promise<void> {
    return new Promise<void>((resolve) => {
      const allImages: NodeListOf<HTMLElement> = el.querySelectorAll('[slot] > img');

      Array.from(allImages).forEach((image: HTMLElement) => {
        if (image.getAttribute('data-src')) {
          image.setAttribute('src', image.getAttribute('data-src'));
          image.removeAttribute('data-src');
        }
      });

      resolve();
    });
  };

}
