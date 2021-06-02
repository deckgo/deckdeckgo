export interface UndoRedoChangeAttribute {
  attribute: string;
  value: string;
  updateUI: (value: string) => void;
}

export interface UndoRedoChangeStyle {
  value: string | null;
  updateUI: (value: string) => Promise<void>;
}

export interface UndoRedoChangeElement {
  innerHTML: string;
}

export interface UndoRedoChange {
  type: 'input' | 'attribute' | 'style';
  target: HTMLElement;
  data: UndoRedoChangeAttribute | UndoRedoChangeElement | UndoRedoChangeStyle;
}
