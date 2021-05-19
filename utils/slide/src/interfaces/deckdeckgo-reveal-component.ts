export interface DeckDeckGoRevealComponent extends HTMLElement {
  reveal(): Promise<void>;
  hide(): Promise<void>;
  revealAll(): Promise<void>;
  hideAll(): Promise<void>;
  allElementsRevealed: boolean;
  allElementsHidden: boolean;
}
