export interface UndoRedoChangeAttribute {
  attribute: string;
  value: string;
  updateUI: (value: string) => void;
}

export interface UndoRedoChangeStyle {
  value: string | null;
  type: 'deck' | 'slide' | 'element';
  updateUI: (value: string) => Promise<void>;
}

export interface UndoRedoChangeElement {
  innerHTML: string;
}

export interface UndoRedoInputElement {
  offset: number;
  oldValue: string;
}

export interface UndoRedoChange {
  type: 'input' | 'attribute' | 'style';
  target: Node;
  data: UndoRedoChangeAttribute | UndoRedoChangeElement | UndoRedoChangeStyle | UndoRedoInputElement;
}
