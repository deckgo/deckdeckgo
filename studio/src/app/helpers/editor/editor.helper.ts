import editorStore from '../../stores/editor.store';

import {UndoRedoChange, UndoRedoChangeAttribute, UndoRedoChangeElement} from '../../types/editor/undo-redo';

export const setAttribute = (target: HTMLElement, {attribute, value, updateUI}: UndoRedoChangeAttribute) => {
  editorStore.state.undo.push({
    type: 'style',
    target,
    data: {
      attribute,
      value: target.getAttribute(attribute),
      updateUI,
    },
  });

  editorStore.state.redo = [];

  target.setAttribute(attribute, value);

  emitDidUpdate({target, eventName: 'deckDidChange'});

  updateUI(value);
};

export const undo = async ($event: KeyboardEvent) => {
  if (editorStore.state.stack !== undefined) {
    return;
  }

  $event.preventDefault();

  const undoChange: UndoRedoChange | undefined = editorStore.state.undo[editorStore.state.undo.length - 1];

  if (!undoChange) {
    return;
  }

  const {type, data, target} = undoChange;

  if (type === 'input') {
    editorStore.state.redo.push({type, target, data: {innerHTML: target.innerHTML}});

    undoRedoElement(target, data as UndoRedoChangeElement);
  }

  if (type === 'style') {
    const {attribute} = data as UndoRedoChangeAttribute;

    editorStore.state.redo.push({
      type,
      target,
      data: {
        ...data,
        value: target.getAttribute(attribute),
      },
    });

    undoRedoSetAttribute(target, data as UndoRedoChangeAttribute);
  }

  editorStore.state.undo = [...editorStore.state.undo.slice(0, editorStore.state.undo.length - 1)];
};

export const redo = async ($event: KeyboardEvent) => {
  if (editorStore.state.stack !== undefined) {
    return;
  }

  $event.preventDefault();

  const redoChange: UndoRedoChange | undefined = editorStore.state.redo[editorStore.state.redo.length - 1];

  if (!redoChange) {
    return;
  }

  const {type, data, target} = redoChange;

  if (type === 'input') {
    editorStore.state.undo.push({type, target, data: {innerHTML: target.innerHTML}});

    undoRedoElement(target, data as UndoRedoChangeElement);
  }

  if (type === 'style') {
    const {attribute} = data as UndoRedoChangeAttribute;

    editorStore.state.undo.push({
      type,
      target,
      data: {
        ...data,
        value: target.getAttribute(attribute),
      },
    });

    undoRedoSetAttribute(target, data as UndoRedoChangeAttribute);
  }

  editorStore.state.redo = [...editorStore.state.redo.slice(0, editorStore.state.redo.length - 1)];
};

const undoRedoElement = (target: HTMLElement, {innerHTML}: UndoRedoChangeElement) => {
  target.innerHTML = innerHTML;

  emitDidUpdate({target: target.parentElement, eventName: 'slideDidChange'});
};

const undoRedoSetAttribute = (target: HTMLElement, {attribute, value, updateUI}: UndoRedoChangeAttribute) => {
  target.setAttribute(attribute, value);

  emitDidUpdate({target, eventName: 'deckDidChange'});

  updateUI(value);
};

const emitDidUpdate = ({eventName, target}: {target: HTMLElement; eventName: 'deckDidChange' | 'slideDidChange'}) => {
  const didUpdate: CustomEvent<HTMLElement> = new CustomEvent<HTMLElement>(eventName, {
    bubbles: true,
    detail: target,
  });

  target.dispatchEvent(didUpdate);
};
