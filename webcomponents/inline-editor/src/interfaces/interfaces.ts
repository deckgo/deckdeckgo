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

export interface ExecCommandAction {
  style: 'color' | 'background-color' | 'font-size' | 'font-weight' | 'font-style' | 'text-decoration';
  value: string;
}
