import undoRedoStore from '../../stores/undo-redo.store';

import {UndoRedoChange, UndoRedoChangeAttribute, UndoRedoChangeElement, UndoRedoChangeStyle} from '../../types/editor/undo-redo';

export const setAttribute = (target: HTMLElement, {attribute, value, updateUI}: UndoRedoChangeAttribute) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  undoRedoStore.state.undo.push({
    type: 'attribute',
    target,
    data: {
      attribute,
      value: target.getAttribute(attribute),
      updateUI
    }
  });

  undoRedoStore.state.redo = [];

  target.setAttribute(attribute, value);

  updateUI(value);
};

export const setStyle = (
  target: HTMLElement,
  {
    properties,
    type,
    updateUI
  }: {
    properties: {property: string; value: string | null}[];
    type: 'deck' | 'slide' | 'element';
    updateUI: (value: string) => Promise<void>;
  }
) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  undoRedoStore.state.undo.push({
    type: 'style',
    target,
    data: {
      value: target.getAttribute('style'),
      type,
      updateUI
    }
  });

  undoRedoStore.state.redo = [];

  properties.forEach(({property, value}) => {
    if (value === null) {
      target.style.removeProperty(property);
      return;
    }

    target.style.setProperty(property, value);
  });
};

export const undo = async ($event: KeyboardEvent) => {
  const result: {from: UndoRedoChange[]} | undefined = await undoRedo({
    from: undoRedoStore.state.undo,
    to: undoRedoStore.state.redo,
    $event
  });

  if (!result) {
    return;
  }

  const {from}: {from: UndoRedoChange[]} = result;

  undoRedoStore.state.undo = from;
};

export const redo = async ($event: KeyboardEvent) => {
  const result: {from: UndoRedoChange[]} | undefined = await undoRedo({
    from: undoRedoStore.state.redo,
    to: undoRedoStore.state.undo,
    $event
  });

  if (!result) {
    return;
  }

  const {from}: {from: UndoRedoChange[]} = result;

  undoRedoStore.state.redo = from;
};

export const undoRedo = async ({
  $event,
  from,
  to
}: {
  from: UndoRedoChange[] | undefined;
  to: UndoRedoChange[] | undefined;
  $event: KeyboardEvent;
}): Promise<{from: UndoRedoChange[]} | undefined> => {
  if (undoRedoStore.state.elementInnerHTML !== undefined) {
    return undefined;
  }

  if (from === undefined || to === undefined) {
    return undefined;
  }

  $event.preventDefault();

  const undoChange: UndoRedoChange | undefined = from[from.length - 1];

  if (!undoChange) {
    return undefined;
  }

  const {type, data, target} = undoChange;

  // In case of decks, we are using HTMLElement only
  const element: HTMLElement = target as HTMLElement;

  if (type === 'input') {
    to.push({type, target, data: {innerHTML: element.innerHTML}});

    undoRedoElement(element, data as UndoRedoChangeElement);
  }

  if (type === 'style') {
    to.push({
      type,
      target,
      data: {
        ...data,
        value: element.getAttribute('style')
      }
    });

    await undoRedoSetStyle(element, data as UndoRedoChangeStyle);
  }

  if (type === 'attribute') {
    const {attribute} = data as UndoRedoChangeAttribute;

    to.push({
      type,
      target,
      data: {
        ...data,
        value: element.getAttribute(attribute)
      }
    });

    undoRedoSetAttribute(element, data as UndoRedoChangeAttribute);
  }

  return {
    from: [...from.slice(0, from.length - 1)]
  };
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
    detail: target
  });

  target.dispatchEvent(didUpdate);
};
