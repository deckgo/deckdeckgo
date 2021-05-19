import {lazyLoadImages} from './images-utils';
import {lazyLoadComponentContent} from './component-utils';

import {DeckDeckGoRevealComponent} from '../interfaces/deckdeckgo-reveal-component';

const revealSelector: string = 'deckgo-reveal, deckgo-reveal-list, deckgo-highlight-code';

async function showRevealElement(el: HTMLElement): Promise<boolean> {
  const elements: NodeListOf<DeckDeckGoRevealComponent> = el.querySelectorAll(revealSelector);

  if (!elements || elements.length <= 0) {
    return true;
  }

  const nextElement: DeckDeckGoRevealComponent | undefined = Array.from(elements).find((element: DeckDeckGoRevealComponent) => {
    return !element.allElementsRevealed;
  });

  if (nextElement) {
    await nextElement.reveal();
    return false;
  }

  return true;
}

async function hideRevealElement(el: HTMLElement): Promise<boolean> {
  const elements: NodeListOf<DeckDeckGoRevealComponent> = el.querySelectorAll(revealSelector);

  if (!elements || elements.length <= 0) {
    return true;
  }

  let couldSwipe: boolean = true;

  const nextElement: DeckDeckGoRevealComponent | undefined = Array.from(elements)
    .reverse()
    .find((element: DeckDeckGoRevealComponent) => {
      return !element.allElementsHidden;
    });

  if (nextElement) {
    await nextElement.hide();
    couldSwipe = false;
  }

  return couldSwipe;
}

export async function showAllRevealElements(el: HTMLElement): Promise<void> {
  const elements: NodeListOf<DeckDeckGoRevealComponent> = el.querySelectorAll(revealSelector);

  if (!elements || elements.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = Array.from(elements).map((element: DeckDeckGoRevealComponent) => element.revealAll());

  await Promise.all(promises);
}

export async function hideAllRevealElements(el: HTMLElement): Promise<void> {
  const elements: NodeListOf<DeckDeckGoRevealComponent> = el.querySelectorAll(revealSelector);

  if (!elements || elements.length <= 0) {
    return;
  }

  const promises: Promise<void>[] = Array.from(elements).map((element: DeckDeckGoRevealComponent) => element.hideAll());

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
