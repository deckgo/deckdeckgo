import {createStore} from '@stencil/store';

import {UndoRedoChange} from '../types/editor/undo-redo';

interface UndoRedoStore {
  undo: UndoRedoChange[] | undefined;
  redo: UndoRedoChange[] | undefined;

  observe: boolean;

  // The innerHTML (clone would work but, we cannot maintain a reference) of the selected element which is about to be edited (text input).
  // On changes, we push the value in the undo stack.
  elementInnerHTML: string | undefined;
}

const {state, onChange, reset} = createStore<UndoRedoStore>({
  undo: undefined,
  redo: undefined,
  observe: true,
  elementInnerHTML: undefined
});

export default {state, onChange, reset};
