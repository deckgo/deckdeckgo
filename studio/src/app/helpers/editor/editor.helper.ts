import editorStore from '../../stores/editor.store';

import { UndoRedoChange } from "../../types/editor/undo-redo";

export const setAttribute = ({element, attribute, value, updateUI}: UndoRedoChange) => {
  editorStore.state.undo.push({
    element,
    attribute,
    value: element.getAttribute(attribute),
    updateUI
  });
  editorStore.state.redo = [];

  element.setAttribute(attribute, value);

  emitDidUpdate({element, eventName: 'deckDidChange'});

  updateUI(value);
}

export const undo = async () => {
  const undoChange: UndoRedoChange | undefined = editorStore.state.undo[editorStore.state.undo.length - 1];
  
  if (!undoChange) {
    return;
  }
  
  const {element, attribute} = undoChange;

  editorStore.state.redo.push({
    ...undoChange,
    value: element.getAttribute(attribute)
  });

  editorStore.state.undo = [...editorStore.state.undo.slice(0, editorStore.state.undo.length - 1)];

  undoRedoSetAttribute(undoChange);
}

export const redo = async () => {
  const redoChange: UndoRedoChange | undefined = editorStore.state.redo[editorStore.state.redo.length - 1];

  if (!redoChange) {
    return;
  }

  const {element, attribute} = redoChange;

  editorStore.state.undo.push({
    ...redoChange,
    value: element.getAttribute(attribute)
  });

  editorStore.state.redo = [...editorStore.state.redo.slice(0, editorStore.state.redo.length - 1)];

  undoRedoSetAttribute(redoChange);
}

const undoRedoSetAttribute = ({element, attribute, value, updateUI}: UndoRedoChange) => {
  element.setAttribute(attribute, value);

  emitDidUpdate({element, eventName: 'deckDidChange'});

  updateUI(value);
}

const emitDidUpdate = ({eventName, element}: {element: HTMLElement, eventName: 'deckDidChange'}) => {
  const didUpdate: CustomEvent<HTMLElement> = new CustomEvent<HTMLElement>(eventName, {
    bubbles: true,
    detail: element,
  });

  element.dispatchEvent(didUpdate);
}
