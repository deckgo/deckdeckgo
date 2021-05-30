import {createStore} from '@stencil/store';

import {UndoRedoChange} from '../types/editor/undo-redo';

interface UndoRedoStore {
  undo: UndoRedoChange[];
  redo: UndoRedoChange[];
  stack: string | undefined;
}

const {state} = createStore<UndoRedoStore>({
  undo: [],
  redo: [],
  stack: undefined,
});

export default {state};
