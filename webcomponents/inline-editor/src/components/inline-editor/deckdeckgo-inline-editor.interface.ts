import { EventEmitter } from '@stencil/core';

export interface AnchorLink {
  range: Range;
  text: string;
  element: Element;
}

export interface InputTargetEvent extends EventTarget {
  value: string;
}

export type InlineActionCommand = 'bold' | 'insertHTML'; // TODO: Add all available

export interface InlineAction {
  command: InlineActionCommand;
  value?: string;
}

export interface InlineActionComponent {
  selection: Selection;
  anchorLink: AnchorLink;
  commandTriggered: EventEmitter<InlineAction>;
}
