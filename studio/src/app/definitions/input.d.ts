interface InputTargetEvent extends EventTarget {
  value: string;
}

interface InputUIEvent extends UIEvent {
  target: HTMLElement;
}
