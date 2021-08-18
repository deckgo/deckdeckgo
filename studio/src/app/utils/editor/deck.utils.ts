export const deckSelector: string = 'app-editor > ion-content div.deck > main > deckgo-deck';

export const slideTo = async (index: number) => {
  const deck: HTMLDeckgoDeckElement | undefined = document.querySelector(deckSelector);
  await deck.slideTo(index);
};
