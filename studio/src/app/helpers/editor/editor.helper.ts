import editorStore from '../../stores/editor.store';

import { UndoRedoChange, UndoRedoChangeAttribute } from "../../types/editor/undo-redo";

export const setAttribute = ({element, attribute, value, updateUI}: UndoRedoChangeAttribute) => {
  editorStore.state.undo.push({
    type: 'style',
    data: {
      element,
      attribute,
      value: element.getAttribute(attribute),
      updateUI
    }
  });

  editorStore.state.redo = [];

  element.setAttribute(attribute, value);

  emitDidUpdate({element, eventName: 'deckDidChange'});

  updateUI(value);
}

export const undo = async ($event: KeyboardEvent) => {
  const undoChange: UndoRedoChange | undefined = editorStore.state.undo[editorStore.state.undo.length - 1];

  if (!undoChange) {
    return;
  }

  const {type, data} = undoChange;

  if (type === 'input') {
    editorStore.state.redo.push({type});
  }

  if (type === 'style') {
    $event.preventDefault();

    const {element, attribute} = data;

    editorStore.state.redo.push({
      type,
      data: {
        ...data,
        value: element.getAttribute(attribute)
      }
    });

    undoRedoSetAttribute(data);
  }

  editorStore.state.undo = [...editorStore.state.undo.slice(0, editorStore.state.undo.length - 1)];
}

export const redo = async ($event: KeyboardEvent) => {
  const redoChange: UndoRedoChange | undefined = editorStore.state.redo[editorStore.state.redo.length - 1];

  if (!redoChange) {
    return;
  }

  const {type, data} = redoChange;

  if (type === 'input') {
    editorStore.state.undo.push({type});
  }

  if (type === 'style') {
    $event.preventDefault();

    const {element, attribute} = data;

    editorStore.state.undo.push({
      type,
      data: {
        ...data,
        value: element.getAttribute(attribute)
      }
    });

    undoRedoSetAttribute(data);
  }

  editorStore.state.redo = [...editorStore.state.redo.slice(0, editorStore.state.redo.length - 1)];
}

const undoRedoSetAttribute = ({element, attribute, value, updateUI}: UndoRedoChangeAttribute) => {
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
