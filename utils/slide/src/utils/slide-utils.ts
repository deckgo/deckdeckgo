import {lazyLoadImages} from './images-utils';
import {lazyLoadComponentContent} from './component-utils';

import {DeckDeckGoRevealComponent} from '../interfaces/deckdeckgo-reveal-component';

const revealSelector: string = 'deckgo-reveal, deckgo-reveal-list, deckgo-highlight-code[highlight-lines]';

async function showRevealElement(el: HTMLElement): Promise<boolean> {
  const elements: NodeListOf<HTMLElement> = el.querySelectorAll(revealSelector);

  if (!elements || elements.length <= 0) {
    return true;
  }

  const nextElement: HTMLElement | undefined = Array.from(elements).find((element: HTMLElement) => {
    return (
      (element as unknown as DeckDeckGoRevealComponent).revealProgress !== 'end' &&
      (element as unknown as DeckDeckGoRevealComponent).revealProgress !== undefined
    );
  });

  if (nextElement) {
    await (nextElement as unknown as DeckDeckGoRevealComponent).reveal();
    return false;
  }

  return true;
}

async function hideRevealElement(el: HTMLElement): Promise<boolean> {
  const elements: NodeListOf<HTMLElement> = el.querySelectorAll(revealSelector);

  if (!elements || elements.length <= 0) {
    return true;
  }

  let couldSwipe: boolean = true;

  const nextElement: HTMLElement | undefined = Array.from(elements)
    .reverse()
    .find((element: HTMLElement) => {
      return (
        (element as unknown as DeckDeckGoRevealComponent).revealProgress !== 'start' &&
        (element as unknown as DeckDeckGoRevealComponent).revealProgress !== undefined
      );
    });

  if (nextElement) {
    await (nextElement as unknown as DeckDeckGoRevealComponent).hide();
    couldSwipe = false;
  }

  return couldSwipe;
}

export async function showAllRevealElements(el: HTMLElement): Promise<void> {
  const elements: NodeListOf<HTMLElement> = el.querySelectorAll(revealSelector);

  if (!elements || elements.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = Array.from(elements).map((element: HTMLElement) =>
    (element as unknown as DeckDeckGoRevealComponent).revealAll()
  );

  await Promise.all(promises);
}

export async function hideAllRevealElements(el: HTMLElement): Promise<void> {
  const elements: NodeListOf<HTMLElement> = el.querySelectorAll(revealSelector);

  if (!elements || elements.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = Array.from(elements).map((element: HTMLElement) =>
    (element as unknown as DeckDeckGoRevealComponent).hideAll()
  );

  await Promise.all(promises);
}

export async function beforeSwipe(el: HTMLElement, enter: boolean, reveal: boolean): Promise<boolean> {
  if (reveal) {
    return enter ? await showRevealElement(el) : await hideRevealElement(el);
  }

  return true;
}

export function afterSwipe(): Promise<void> {
  return Promise.resolve();
}

export async function lazyLoadContent(el: HTMLElement): Promise<void> {
  const promises: Promise<void>[] = [];

  promises.push(lazyLoadImages(el));
  promises.push(lazyLoadComponentContent(el, 'deckgo-gif'));
  promises.push(lazyLoadComponentContent(el, 'deckgo-youtube'));
  promises.push(lazyLoadComponentContent(el, 'deckgo-demo'));
  promises.push(lazyLoadComponentContent(el, 'deckgo-word-cloud'));
  promises.push(lazyLoadComponentContent(el, 'deckgo-markdown'));

  await Promise.all(promises);
}
