export interface AnchorLink {
  range: Range;
  text: string;
  element: Element;
}

export interface InputTargetEvent extends EventTarget {
  value: string;
}

export interface InlineAction {
  action: string;
  selection: Selection;
  anchorLink: AnchorLink;
}
