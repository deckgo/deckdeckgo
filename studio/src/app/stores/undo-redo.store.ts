import {createStore} from '@stencil/store';

import {UndoRedoChange} from '../types/editor/undo-redo';

interface UndoRedoStore {
  undo: UndoRedoChange[];
  redo: UndoRedoChange[];

  // The innerHTML (clone would work but, we cannot maintain a reference) of the selected element which is about to be edited (text input).
  // On changes, we push the value in the undo stack.
  elementInnerHTML: string | undefined;
}

const {state} = createStore<UndoRedoStore>({
  undo: [],
  redo: [],
  elementInnerHTML: undefined,
});

export default {state};
