import { isMobile } from "@deckdeckgo/utils";

export function isPapyrus(deck: HTMLDeckgoDeckElement): boolean {
  const mobile = isMobile();
  return (deck.direction === 'papyrus' && !mobile) || (deck.directionMobile === 'papyrus' && mobile);
}

export function isVertical(deck: HTMLDeckgoDeckElement): boolean {
  const mobile = isMobile();
  return (deck.direction === 'vertical' && !mobile) || (deck.directionMobile === 'vertical' && mobile);
}
