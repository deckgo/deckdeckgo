export interface AnchorLink {
  range: Range;
  text: string;
}

export interface InputTargetEvent extends EventTarget {
  value: string;
}
