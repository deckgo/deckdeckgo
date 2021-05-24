export interface UndoRedoChangeAttribute {
  element: HTMLElement;
  attribute: string;
  value: string;
  updateUI: (value: string) => void;
}

export interface UndoRedoChange {
  type: 'input' | 'style';
  data?: UndoRedoChangeAttribute;
}
