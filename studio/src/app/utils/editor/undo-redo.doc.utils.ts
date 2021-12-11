import {moveCursorToEnd, moveCursorToOffset} from '@deckdeckgo/utils';

import undoRedoStore from '../../stores/undo-redo.store';

import {UndoRedoChange, UndoRedoDocAddRemoveParagraph, UndoRedoDocInput, UndoRedoDocUpdateParagraph} from '../../types/editor/undo-redo';
import {isTextNode, nodeIndex} from '@deckdeckgo/editor';
import {NodeUtils} from './node.utils';

export const stackUndoInput = ({container, data}: {container: HTMLElement; data: UndoRedoDocInput}) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  undoRedoStore.state.undo.push({
    type: 'input',
    target: container,
    data
  });

  undoRedoStore.state.redo = [];
};

export const stackUndoParagraph = ({
  container,
  changes
}: {
  container: HTMLElement;
  changes: {paragraph: HTMLElement; previousSibling?: HTMLElement; mutation: 'remove' | 'add'}[];
}) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  undoRedoStore.state.undo.push({
    type: 'paragraph',
    target: container,
    data: changes.map(
      ({paragraph, previousSibling, mutation}: {paragraph: HTMLElement; previousSibling?: HTMLElement; mutation: 'remove' | 'add'}) => ({
        outerHTML: paragraph.outerHTML,
        mutation,
        index: previousSibling ? nodeIndex(previousSibling) + 1 : 0
      })
    )
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
    data: paragraphs
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
  let parent: HTMLElement | undefined =
    indexDepths.length <= 0
      ? NodeUtils.toHTMLElement(paragraph)
      : paragraph?.querySelector(`* ${indexDepths.map((depth: number) => `> :nth-child(${depth + 1})`).join(' ')}`);

  if (!parent) {
    // We try to find sibling in case the parent does not yet exist. If we find it, we can replicate such parent for the new text.
    // Useful notably when reverting lists and li.
    const sibling: HTMLElement | undefined = paragraph?.querySelector(
      `* ${indexDepths.map((depth: number) => `> :nth-child(${depth})`).join(' ')}`
    );

    const anchor: Element | undefined = sibling || paragraph.lastElementChild;

    if (!anchor) {
      return;
    }

    parent = anchor.cloneNode() as HTMLElement;
    anchor.after(parent);
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

  const paragraphs: UndoRedoDocAddRemoveParagraph[] = data as UndoRedoDocAddRemoveParagraph[];

  onMutationMoveCursor({container, paragraphs});

  const to: UndoRedoDocAddRemoveParagraph[] = [];

  paragraphs.forEach(({index, outerHTML, mutation}: UndoRedoDocAddRemoveParagraph) => {
    if (mutation === 'add') {
      const element: Element | undefined = container.children[index];

      element?.parentElement.removeChild(element);

      to.push({
        outerHTML,
        index: index - 1,
        mutation: 'remove'
      });
    }

    if (mutation === 'remove') {
      // Paragraph are elements
      container.children[Math.min(index, container.children.length - 1)].insertAdjacentHTML('afterend', outerHTML);

      to.push({
        outerHTML,
        mutation: 'add',
        index: index + 1
      });
    }
  });

  pushTo({
    ...undoChange,
    data: to
  });

  popFrom();
};

const onMutationMoveCursor = ({container, paragraphs}: {container: HTMLElement; paragraphs: UndoRedoDocAddRemoveParagraph[]}) => {
  // We assume the first new paragraph is the one to focus
  const mutation: 'add' | 'remove' = paragraphs[paragraphs.length - 1].mutation;

  const changeObserver: MutationObserver = new MutationObserver((mutations: MutationRecord[]) => {
    changeObserver.disconnect();

    if (mutation === 'add') {
      moveCursorToEnd(mutations[0].previousSibling);
    } else if (mutation === 'remove') {
      moveCursorToEnd(mutations[0].addedNodes[0]);
    }
  });

  changeObserver.observe(container, {childList: true, subtree: true});
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

  const paragraphs: UndoRedoDocUpdateParagraph[] = data as UndoRedoDocUpdateParagraph[];

  const container: HTMLElement = NodeUtils.toHTMLElement(target);

  const to: UndoRedoDocUpdateParagraph[] = [];

  paragraphs.forEach(({index, outerHTML}: UndoRedoDocUpdateParagraph) => {
    const paragraph: Element = container.children[Math.min(index, container.children.length - 1)];

    to.push({index, outerHTML: paragraph.outerHTML});

    paragraph.outerHTML = outerHTML;
  });

  pushTo({
    ...undoChange,
    data: to
  });

  popFrom();
};
