import {moveCursorToOffset} from '@deckdeckgo/utils';

import undoRedoStore from '../../stores/undo-redo.store';

import {UndoRedoChange, UndoRedoInputElement} from '../../types/editor/undo-redo';

export const stackUndoInput = ({mutation, caretPosition}: {mutation: MutationRecord; caretPosition: number}) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  const {oldValue, target} = mutation;

  undoRedoStore.state.undo.push({
    type: 'input',
    target: target,
    data: {oldValue, offset: caretPosition}
  });

  undoRedoStore.state.redo = [];
};

export const nextUndoChange = (): UndoRedoChange | undefined => nextChange(undoRedoStore.state.undo);

export const nextRedoChange = (): UndoRedoChange | undefined => nextChange(undoRedoStore.state.redo);

const nextChange = (changes: UndoRedoChange[] | undefined): UndoRedoChange | undefined => {
  if (!changes) {
    return undefined;
  }

  return changes[changes.length - 1];
};

export const undo = () =>
  undoRedo({
    popFrom: () => (undoRedoStore.state.undo = [...undoRedoStore.state.undo.slice(0, undoRedoStore.state.undo.length - 1)]),
    pushTo: (value: UndoRedoChange) => undoRedoStore.state.redo.push(value),
    undoChange: nextUndoChange()
  });

export const redo = () =>
  undoRedo({
    popFrom: () => (undoRedoStore.state.redo = [...undoRedoStore.state.redo.slice(0, undoRedoStore.state.redo.length - 1)]),
    pushTo: (value: UndoRedoChange) => undoRedoStore.state.undo.push(value),
    undoChange: nextRedoChange()
  });

const undoRedo = ({
  popFrom,
  pushTo,
  undoChange
}: {
  popFrom: () => void;
  pushTo: (value: UndoRedoChange) => void;
  undoChange: UndoRedoChange | undefined;
}) => {
  if (!undoChange) {
    return;
  }

  const {type, data, target} = undoChange;

  if (type === 'input') {
    const currentValue: string = target.nodeValue;

    const {oldValue, offset: newCaretPosition} = data as UndoRedoInputElement;

    const changeObserver: MutationObserver = new MutationObserver(() => {
      changeObserver.disconnect();

      moveCursorToOffset({element: target, offset: newCaretPosition});

      pushTo({
        type: 'input',
        target: target as HTMLElement,
        data: {oldValue: currentValue, offset: newCaretPosition + (currentValue.length - oldValue.length)}
      });

      popFrom();
    });

    changeObserver.observe(target, {characterData: true, subtree: true});

    target.nodeValue = oldValue;
  }
};
