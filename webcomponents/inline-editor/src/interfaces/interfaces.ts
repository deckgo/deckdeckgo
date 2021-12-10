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

export interface ExecCommandStyle {
  style: 'color' | 'background-color' | 'font-size' | 'font-weight' | 'font-style' | 'text-decoration';
  value: string;
  initial: (element: HTMLElement | null) => boolean;
}

export interface ExecCommandList {
  type: 'ol' | 'ul';
}

export interface ExecCommandAction {
  cmd: 'style' | 'list';
  detail: ExecCommandStyle | ExecCommandList;
}
