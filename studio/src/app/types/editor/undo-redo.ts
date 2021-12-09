export interface UndoRedoDeckChangeAttribute {
  attribute: string;
  value: string;
  updateUI: (value: string) => void;
}

export interface UndoRedoDeckChangeStyle {
  value: string | null;
  type: 'deck' | 'slide' | 'element';
  updateUI: (value: string) => Promise<void>;
}

export interface UndoRedoDeckInputElement {
  innerHTML: string;
}

export interface UndoRedoDocInput {
  offset: number;
  oldValue: string;
}

export interface UndoRedoDocParagraph {
  container: HTMLElement;
  index: number;
  mutation: 'add' | 'remove';
  outerHTML: string;
}

export interface UndoRedoChange {
  type: 'input' | 'attribute' | 'style' | 'paragraph';
  target: Node;
  data: UndoRedoDeckChangeAttribute | UndoRedoDeckInputElement | UndoRedoDeckChangeStyle | UndoRedoDocInput | UndoRedoDocParagraph;
}
