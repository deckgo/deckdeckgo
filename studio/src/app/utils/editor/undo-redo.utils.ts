import undoRedoStore from '../../stores/undo-redo.store';

import {UndoRedoChange, UndoRedoChangeAttribute, UndoRedoChangeElement, UndoRedoChangeStyle} from '../../types/editor/undo-redo';

export const setAttribute = (target: HTMLElement, {attribute, value, updateUI}: UndoRedoChangeAttribute) => {
  undoRedoStore.state.undo.push({
    type: 'attribute',
    target,
    data: {
      attribute,
      value: target.getAttribute(attribute),
      updateUI,
    },
  });

  undoRedoStore.state.redo = [];

  target.setAttribute(attribute, value);

  emitDidUpdate({target, eventName: 'deckDidChange'});

  updateUI(value);
};

export const setStyle = (target: HTMLElement, property: string, {value, type, updateUI}: UndoRedoChangeStyle) => {
  undoRedoStore.state.undo.push({
    type: 'style',
    target,
    data: {
      value: target.getAttribute('style'),
      type,
      updateUI,
    },
  });

  undoRedoStore.state.redo = [];

  if (value === null) {
    target.style.removeProperty(property);
    return;
  }

  target.style.setProperty(property, value);
};

export const undo = async ($event: KeyboardEvent) => {
  if (undoRedoStore.state.elementInnerHTML !== undefined) {
    return;
  }

  $event.preventDefault();

  const undoChange: UndoRedoChange | undefined = undoRedoStore.state.undo[undoRedoStore.state.undo.length - 1];

  if (!undoChange) {
    return;
  }

  const {type, data, target} = undoChange;

  if (type === 'input') {
    undoRedoStore.state.redo.push({type, target, data: {innerHTML: target.innerHTML}});

    undoRedoElement(target, data as UndoRedoChangeElement);
  }

  if (type === 'style') {
    undoRedoStore.state.redo.push({
      type,
      target,
      data: {
        ...data,
        value: target.getAttribute('style'),
      },
    });

    await undoRedoSetStyle(target, data as UndoRedoChangeStyle);
  }

  if (type === 'attribute') {
    const {attribute} = data as UndoRedoChangeAttribute;

    undoRedoStore.state.redo.push({
      type,
      target,
      data: {
        ...data,
        value: target.getAttribute(attribute),
      },
    });

    undoRedoSetAttribute(target, data as UndoRedoChangeAttribute);
  }

  undoRedoStore.state.undo = [...undoRedoStore.state.undo.slice(0, undoRedoStore.state.undo.length - 1)];
};

export const redo = async ($event: KeyboardEvent) => {
  if (undoRedoStore.state.elementInnerHTML !== undefined) {
    return;
  }

  $event.preventDefault();

  const redoChange: UndoRedoChange | undefined = undoRedoStore.state.redo[undoRedoStore.state.redo.length - 1];

  if (!redoChange) {
    return;
  }

  const {type, data, target} = redoChange;

  if (type === 'input') {
    undoRedoStore.state.undo.push({type, target, data: {innerHTML: target.innerHTML}});

    undoRedoElement(target, data as UndoRedoChangeElement);
  }

  if (type === 'style') {
    undoRedoStore.state.undo.push({
      type,
      target,
      data: {
        ...data,
        value: target.getAttribute('style'),
      },
    });

    await undoRedoSetStyle(target, data as UndoRedoChangeStyle);
  }

  if (type === 'attribute') {
    const {attribute} = data as UndoRedoChangeAttribute;

    undoRedoStore.state.undo.push({
      type,
      target,
      data: {
        ...data,
        value: target.getAttribute(attribute),
      },
    });

    undoRedoSetAttribute(target, data as UndoRedoChangeAttribute);
  }

  undoRedoStore.state.redo = [...undoRedoStore.state.redo.slice(0, undoRedoStore.state.redo.length - 1)];
};

const undoRedoElement = (target: HTMLElement, {innerHTML}: UndoRedoChangeElement) => {
  target.innerHTML = innerHTML;

  emitDidUpdate({target: target.parentElement, eventName: 'slideDidChange'});
};

const undoRedoSetStyle = async (target: HTMLElement, {value, type, updateUI}: UndoRedoChangeStyle) => {
  target.setAttribute('style', value);

  if (type === 'deck') {
    emitDidUpdate({target, eventName: 'deckDidChange'});
  } else if (type === 'slide') {
    emitDidUpdate({target, eventName: 'slideDidChange'});
  } else {
    emitDidUpdate({target: target.parentElement, eventName: 'slideDidChange'});
  }

  await updateUI(value);
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
