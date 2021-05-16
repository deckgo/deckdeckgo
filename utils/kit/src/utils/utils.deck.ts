import {isMobile} from '@deckdeckgo/utils';

export function isPapyrus(deck: HTMLDeckgoDeckElement): boolean {
  const mobile = isMobile();
  return (deck.direction === 'papyrus' && !mobile) || (deck.directionMobile === 'papyrus' && mobile);
}

export function isVertical(deck: HTMLDeckgoDeckElement): boolean {
  const mobile = isMobile();
  return (deck.direction === 'vertical' && !mobile) || (deck.directionMobile === 'vertical' && mobile);
}

export function isScreenshot(): boolean {
  const url: URL | undefined = new URL(window?.location?.href);
  return url && url.searchParams ? url.searchParams.has('screenshot') : false;
}
