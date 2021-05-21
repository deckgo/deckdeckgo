export interface DeckDeckGoRevealComponent {
  reveal(): Promise<void>;
  hide(): Promise<void>;
  revealAll(): Promise<void>;
  hideAll(): Promise<void>;
  revealProgress: 'start' | 'partial' | 'end';
}
