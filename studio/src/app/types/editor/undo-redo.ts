export interface UndoRedoChange {
  element: HTMLElement;
  attribute: string;
  value: string;
  updateUI: (value: string) => void;
}
