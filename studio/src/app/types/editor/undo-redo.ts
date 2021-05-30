export interface UndoRedoChangeAttribute {
  attribute: string;
  value: string;
  updateUI: (value: string) => void;
}

export interface UndoRedoChangeElement {
  innerHTML: string;
}

export interface UndoRedoChange {
  type: 'input' | 'style';
  target: HTMLElement;
  data: UndoRedoChangeAttribute | UndoRedoChangeElement;
}
