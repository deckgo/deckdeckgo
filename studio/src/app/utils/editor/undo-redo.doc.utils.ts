import {moveCursorToEnd, moveCursorToOffset} from '@deckdeckgo/utils';

import undoRedoStore from '../../stores/undo-redo.store';

import {UndoRedoChange, UndoRedoDocParagraph, UndoRedoDocInput} from '../../types/editor/undo-redo';
import {nodeIndex} from '@deckdeckgo/editor';

export const stackUndoInput = ({mutation, caretPosition}: {mutation: MutationRecord; caretPosition: number}) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  const {oldValue, target} = mutation;

  undoRedoStore.state.undo.push({
    type: 'input',
    target,
    data: {oldValue, offset: caretPosition}
  });

  undoRedoStore.state.redo = [];
};

export const stackUndoParagraph = ({
  paragraph,
  mutation,
  container,
  previousSibling
}: {
  paragraph: HTMLElement;
  container: HTMLElement;
  mutation: 'add' | 'remove';
  previousSibling: HTMLElement;
}) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  undoRedoStore.state.undo.push({
    type: 'paragraph',
    target: paragraph,
    data: {outerHTML: paragraph.outerHTML, index: nodeIndex(previousSibling) + 1, mutation, container}
  });

  if (!undoRedoStore.state.redo) {
    undoRedoStore.state.redo = [];
  }
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

  const {type} = undoChange;

  if (type === 'input') {
    undoRedoInput({popFrom, pushTo, undoChange});
  }

  if (type === 'paragraph') {
    undoRedoParagraph({popFrom, pushTo, undoChange});
  }
};

const undoRedoInput = ({
  popFrom,
  pushTo,
  undoChange
}: {
  popFrom: () => void;
  pushTo: (value: UndoRedoChange) => void;
  undoChange: UndoRedoChange;
}) => {
  const {data, target} = undoChange;

  const currentValue: string = target.nodeValue;

  const {oldValue, offset: newCaretPosition} = data as UndoRedoDocInput;

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
};

const undoRedoParagraph = ({
  popFrom,
  pushTo,
  undoChange
}: {
  popFrom: () => void;
  pushTo: (value: UndoRedoChange) => void;
  undoChange: UndoRedoChange;
}) => {
  const {data} = undoChange;

  const {index, outerHTML, mutation, container} = data as UndoRedoDocParagraph;

  if (mutation === 'add') {
    // Paragraph are elements

    const element: Element | undefined = container.children[index];

    if (!element) {
      return;
    }

    const changeObserver: MutationObserver = new MutationObserver((mutations: MutationRecord[]) => {
      changeObserver.disconnect();

      moveCursorToEnd(mutations[0].previousSibling);

      pushTo({
        ...undoChange,
        data: {
          ...data,
          index: index - 1,
          mutation: 'remove'
        }
      });

      popFrom();
    });

    changeObserver.observe(container, {childList: true, subtree: true});

    element.parentElement.removeChild(element);

    return;
  }

  if (mutation === 'remove') {
    const changeObserver: MutationObserver = new MutationObserver((mutations: MutationRecord[]) => {
      changeObserver.disconnect();

      moveCursorToEnd(mutations[0].addedNodes[0]);

      pushTo({
        ...undoChange,
        data: {
          ...data,
          mutation: 'add',
          index: index + 1
        }
      });

      popFrom();
    });

    changeObserver.observe(container, {childList: true, subtree: true});

    // Paragraph are elements
    container.children[Math.min(index, container.children.length - 1)].insertAdjacentHTML('afterend', outerHTML);
  }
};
