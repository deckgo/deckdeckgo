interface HTMLSlideVideoElement extends HTMLElement {
  play(): Promise<void>;
  pause(): Promise<void>;
}
