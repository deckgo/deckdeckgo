import {lazyLoadImages} from './images-utils';
import {lazyLoadComponentContent} from './component-utils';

function showRevealElement(el: HTMLElement): Promise<boolean> {
  return new Promise<boolean>(async (resolve) => {
    const elements: NodeListOf<HTMLElement> = el.querySelectorAll('deckgo-reveal, deckgo-reveal-list');

    let couldSwipe: boolean = true;

    if (elements) {
      const nextElement: HTMLElement | undefined = Array.from(elements).find((element: HTMLElement) => {
        return !(element as any).allElementsRevealed;
      });

      if (nextElement) {
        await (nextElement as any).reveal();
        couldSwipe = false;
      }
    }

    resolve(couldSwipe);
  });
}

function hideRevealElement(el: HTMLElement): Promise<boolean> {
  return new Promise<boolean>(async (resolve) => {
    const elements: NodeListOf<HTMLElement> = el.querySelectorAll('deckgo-reveal, deckgo-reveal-list');

    let couldSwipe: boolean = true;

    if (elements) {
      const nextElement: HTMLElement | undefined = Array.from(elements)
        .reverse()
        .find((element: HTMLElement) => {
          return !(element as any).allElementsHidden;
        });

      if (nextElement) {
        await (nextElement as any).hide();
        couldSwipe = false;
      }
    }

    resolve(couldSwipe);
  });
}

export function showAllRevealElements(el: HTMLElement): Promise<void> {
  return new Promise<void>(async (resolve) => {
    const elements: NodeListOf<HTMLElement> = el.querySelectorAll('deckgo-reveal, deckgo-reveal-list');

    if (elements && elements.length > 0) {
      const promises: Promise<void>[] = [];

      for (const element of Array.from(elements)) {
        promises.push((element as any).revealAll());
      }

      await Promise.all(promises);
    }

    resolve();
  });
}

export function hideAllRevealElements(el: HTMLElement): Promise<void> {
  return new Promise<void>(async (resolve) => {
    const elements: NodeListOf<HTMLElement> = el.querySelectorAll('deckgo-reveal, deckgo-reveal-list');

    if (elements && elements.length > 0) {
      const promises: Promise<void>[] = [];

      for (const element of Array.from(elements)) {
        promises.push((element as any).hideAll());
      }

      await Promise.all(promises);
    }

    resolve();
  });
}

export function beforeSwipe(el: HTMLElement, enter: boolean, reveal: boolean): Promise<boolean> {
  return new Promise<boolean>(async (resolve) => {
    if (reveal) {
      const couldSwipe: boolean = enter ? await showRevealElement(el) : await hideRevealElement(el);
      resolve(couldSwipe);
    } else {
      resolve(true);
    }
  });
}

export function afterSwipe(): Promise<void> {
  return new Promise<void>((resolve) => {
    resolve();
  });
}

export function lazyLoadContent(el: HTMLElement): Promise<void> {
  return new Promise<void>(async (resolve) => {
    const promises: Promise<void>[] = [];

    promises.push(lazyLoadImages(el));
    promises.push(lazyLoadComponentContent(el, 'deckgo-gif'));
    promises.push(lazyLoadComponentContent(el, 'deckgo-youtube'));
    promises.push(lazyLoadComponentContent(el, 'deckgo-demo'));
    promises.push(lazyLoadComponentContent(el, 'deckgo-word-cloud'));
    promises.push(lazyLoadComponentContent(el, 'deckgo-markdown'));

    await Promise.all(promises);

    resolve();
  });
}
