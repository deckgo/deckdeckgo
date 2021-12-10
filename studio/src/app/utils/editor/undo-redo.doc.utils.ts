import {moveCursorToEnd, moveCursorToOffset} from '@deckdeckgo/utils';

import undoRedoStore from '../../stores/undo-redo.store';

import {UndoRedoChange, UndoRedoDocParagraph, UndoRedoDocInput, UndoRedoDocUpdate} from '../../types/editor/undo-redo';
import {isTextNode, nodeIndex} from '@deckdeckgo/editor';
import {NodeUtils} from './node.utils';

export const stackUndoInput = ({
  oldValue,
  caretPosition,
  container,
  indexDepths,
  index
}: {
  oldValue: string;
  caretPosition: number;
  container: HTMLElement;
  indexDepths: number[];
  index: number;
}) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  undoRedoStore.state.undo.push({
    type: 'input',
    target: container,
    data: {oldValue, offset: caretPosition, indexDepths, index}
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
  previousSibling?: HTMLElement;
}) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  undoRedoStore.state.undo.push({
    type: 'paragraph',
    target: container,
    data: {outerHTML: paragraph.outerHTML, index: previousSibling ? nodeIndex(previousSibling) + 1 : 0, mutation}
  });

  if (!undoRedoStore.state.redo) {
    undoRedoStore.state.redo = [];
  }
};

export const stackUndoUpdate = ({paragraphs, container}: {paragraphs: {outerHTML: string; index: number}[]; container: HTMLElement}) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  undoRedoStore.state.undo.push({
    type: 'update',
    target: container,
    data: {paragraphs}
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

  if (type === 'update') {
    undoRedoUpdate({popFrom, pushTo, undoChange});
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

  const container: HTMLElement = NodeUtils.toHTMLElement(target);

  const {oldValue, offset: newCaretPosition, index, indexDepths} = data as UndoRedoDocInput;

  const paragraph: Element | undefined = container.children[index];

  // Example: document.querySelector('[paragraph_id="cf914fa8-ff4d-48e9-972d-ecf795af316c"]').querySelector('* > :nth-child(1) > :nth-child(1) > :nth-child(1)')
  const parent: HTMLElement | undefined =
    indexDepths.length <= 0
      ? NodeUtils.toHTMLElement(paragraph)
      : paragraph?.querySelector(`* ${indexDepths.map((depth: number) => `> :nth-child(${depth + 1})`).join(' ')}`);

  if (!parent) {
    return;
  }

  let text: Node | undefined = (parent?.childNodes !== undefined ? Array.from(parent.childNodes) : []).find((node: Node) =>
    isTextNode(node)
  );

  // The text node to apply the input might not exist anymore
  if (!text) {
    text = document.createTextNode('');
    parent.prepend(text);
  }

  const currentValue: string = text.nodeValue;

  const changeObserver: MutationObserver = new MutationObserver(() => {
    changeObserver.disconnect();

    moveCursorToOffset({element: text, offset: newCaretPosition});

    pushTo({
      type: 'input',
      target: container,
      data: {
        index,
        indexDepths,
        oldValue: currentValue,
        offset: newCaretPosition + (currentValue.length - oldValue.length)
      }
    });

    popFrom();
  });

  changeObserver.observe(target, {characterData: true, subtree: true});

  text.nodeValue = oldValue;
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
  const {data, target} = undoChange;

  const container: HTMLElement = NodeUtils.toHTMLElement(target);

  const {index, outerHTML, mutation} = data as UndoRedoDocParagraph;

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

const undoRedoUpdate = ({
  popFrom,
  pushTo,
  undoChange
}: {
  popFrom: () => void;
  pushTo: (value: UndoRedoChange) => void;
  undoChange: UndoRedoChange;
}) => {
  const {data, target} = undoChange;

  const container: HTMLElement = NodeUtils.toHTMLElement(target);

  const {paragraphs} = data as UndoRedoDocUpdate;

  const to: {index: number; outerHTML: string}[] = [];

  paragraphs.forEach(({index, outerHTML}: {index: number; outerHTML: string}) => {
    const paragraph: Element = container.children[Math.min(index, container.children.length - 1)];

    to.push({index, outerHTML: paragraph.outerHTML});

    paragraph.outerHTML = outerHTML;
  });

  pushTo({
    ...undoChange,
    data: {
      paragraphs: to
    }
  });

  popFrom();
};
