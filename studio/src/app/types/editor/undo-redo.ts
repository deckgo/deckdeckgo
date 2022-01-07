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

export interface UndoRedoChange {
  type: 'input' | 'attribute' | 'style';
  target: Node;
  data: UndoRedoDeckChangeAttribute | UndoRedoDeckInputElement | UndoRedoDeckChangeStyle;
}
