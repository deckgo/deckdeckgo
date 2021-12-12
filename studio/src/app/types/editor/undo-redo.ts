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
  index: number;
  indexDepths: number[];
}

export interface UndoRedoDocAddRemoveParagraph {
  index: number;
  mutation: 'add' | 'remove';
  outerHTML: string;
}

export interface UndoRedoDocUpdateParagraph {
  outerHTML: string;
  index: number;
}

export interface UndoRedoChange {
  type: 'input' | 'attribute' | 'style' | 'paragraph' | 'update';
  target: Node;
  data:
    | UndoRedoDeckChangeAttribute
    | UndoRedoDeckInputElement
    | UndoRedoDeckChangeStyle
    | UndoRedoDocInput
    | UndoRedoDocAddRemoveParagraph[]
    | UndoRedoDocUpdateParagraph[];
}
