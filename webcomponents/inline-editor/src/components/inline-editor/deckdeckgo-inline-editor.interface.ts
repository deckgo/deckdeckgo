import { EventEmitter } from '@stencil/core';

export interface AnchorLink {
  range: Range;
  text: string;
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
  actionTriggered: EventEmitter<InlineAction>;
}
