import {moveCursorToEnd, moveCursorToOffset} from '@deckdeckgo/utils';
import {isTextNode} from '@deckdeckgo/editor';

import undoRedoStore from '../../stores/undo-redo.store';

import {UndoRedoChange, UndoRedoDocAddRemoveParagraph, UndoRedoDocInput, UndoRedoDocUpdateParagraph} from '../../types/editor/undo-redo';

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

export const stackUndoParagraph = ({container, changes}: {container: HTMLElement; changes: UndoRedoDocAddRemoveParagraph[]}) => {
  if (!undoRedoStore.state.undo) {
    undoRedoStore.state.undo = [];
  }

  undoRedoStore.state.undo.push({
    type: 'paragraph',
    target: container,
    data: changes.map(({outerHTML, index, mutation}: UndoRedoDocAddRemoveParagraph) => ({
      outerHTML,
      mutation,
      index
    }))
  });

  if (!undoRedoStore.state.redo) {
    undoRedoStore.state.redo = [];
  }
};

export const stackUndoUpdate = ({paragraphs, container}: {paragraphs: {outerHTML: string; index: number}[]; container: HTMLElement}) => {
  if (paragraphs.length <= 0) {
    return;
  }

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

export const undo = async () =>
  undoRedo({
    popFrom: () => (undoRedoStore.state.undo = [...undoRedoStore.state.undo.slice(0, undoRedoStore.state.undo.length - 1)]),
    pushTo: (value: UndoRedoChange) => undoRedoStore.state.redo.push(value),
    undoChange: nextUndoChange()
  });

export const redo = async () =>
  undoRedo({
    popFrom: () => (undoRedoStore.state.redo = [...undoRedoStore.state.redo.slice(0, undoRedoStore.state.redo.length - 1)]),
    pushTo: (value: UndoRedoChange) => undoRedoStore.state.undo.push(value),
    undoChange: nextRedoChange()
  });

const undoRedo = async ({
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
    await undoRedoInput({popFrom, pushTo, undoChange});
  }

  if (type === 'paragraph') {
    await undoRedoParagraph({popFrom, pushTo, undoChange});
  }

  if (type === 'update') {
    await undoRedoUpdate({popFrom, pushTo, undoChange});
  }
};

const undoRedoInput = async ({
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

  const findInputNode = ({parent, indexDepths}: {parent: Node | undefined; indexDepths: number[]}): Node | undefined => {
    const childNode: ChildNode | undefined = (Array.from(parent?.childNodes) || [])[indexDepths[0]];

    if (!childNode) {
      return undefined;
    }

    const [, ...rest] = indexDepths;

    if (rest?.length <= 0) {
      return childNode;
    }

    return findInputNode({parent: childNode, indexDepths: rest});
  };

  let text: Node | undefined = findInputNode({parent: paragraph, indexDepths});

  if (!text || !isTextNode(text)) {
    // We try to find sibling in case the parent does not yet exist. If we find it, we can replicate such parent for the new text.
    // Useful notably when reverting lists and li.
    const cloneIndexDepths: number[] = [...indexDepths];
    cloneIndexDepths.pop();

    let parent: Node | undefined =
      cloneIndexDepths.length <= 0 ? text.parentNode : findInputNode({parent: paragraph, indexDepths: [...cloneIndexDepths]});

    if (!parent) {
      parent = await createLast({paragraph: (paragraph as HTMLElement) || container, container});
    }

    text = await prependText({parent: NodeUtils.toHTMLElement(parent), container});
  }

  const {previousValue} = await updateNodeValue({text, oldValue, container});

  moveCursorToOffset({element: text, offset: newCaretPosition});

  pushTo({
    type: 'input',
    target: container,
    data: {
      index,
      indexDepths,
      oldValue: previousValue,
      offset: newCaretPosition + (previousValue.length - oldValue.length)
    }
  });

  popFrom();
};

const undoRedoParagraph = async ({
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

  for (const paragraph of paragraphs) {
    const {index, outerHTML, mutation} = paragraph;

    if (mutation === 'add') {
      await removeNode({container, index});

      to.push({
        outerHTML,
        index,
        mutation: 'remove'
      });
    }

    if (mutation === 'remove') {
      await insertNode({container, index, outerHTML});

      to.push({
        outerHTML,
        mutation: 'add',
        index
      });
    }
  }

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

const undoRedoUpdate = async ({
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

  for (const paragraph of paragraphs) {
    const {index, outerHTML} = paragraph;

    const {previousOuterHTML} = await updateNode({container, index, outerHTML});
    to.push({index, outerHTML: previousOuterHTML});
  }

  pushTo({
    ...undoChange,
    data: to
  });

  popFrom();
};

/**
 * Because we are using indexes to add or remove back and forth elements, we have to wait for changes to be applied to the DOM before iterating to next element to process.
 * That's why the mutation observer and promises.
 */

const insertNode = ({container, index, outerHTML}: {outerHTML: string; index: number; container: HTMLElement}): Promise<void> =>
  new Promise<void>((resolve) => {
    const changeObserver: MutationObserver = new MutationObserver(() => {
      changeObserver.disconnect();

      resolve();
    });

    changeObserver.observe(container, {childList: true, subtree: true});

    const previousSiblingIndex: number = index - 1;
    container.children[Math.min(previousSiblingIndex, container.children.length - 1)].insertAdjacentHTML('afterend', outerHTML);
  });

const removeNode = ({container, index}: {index: number; container: HTMLElement}): Promise<void> =>
  new Promise<void>((resolve) => {
    const changeObserver: MutationObserver = new MutationObserver(() => {
      changeObserver.disconnect();

      resolve();
    });

    changeObserver.observe(container, {childList: true, subtree: true});

    const element: Element | undefined = container.children[Math.min(index, container.children.length - 1)];
    element?.parentElement.removeChild(element);
  });

const updateNode = ({
  container,
  index,
  outerHTML
}: {
  outerHTML: string;
  index: number;
  container: HTMLElement;
}): Promise<{previousOuterHTML: string}> =>
  new Promise<{previousOuterHTML: string}>((resolve) => {
    const paragraph: Element = container.children[Math.min(index, container.children.length - 1)];

    const previousOuterHTML: string = paragraph.outerHTML;

    const changeObserver: MutationObserver = new MutationObserver(() => {
      changeObserver.disconnect();

      resolve({previousOuterHTML});
    });

    changeObserver.observe(container, {childList: true, subtree: true});

    paragraph.outerHTML = outerHTML;
  });

const prependText = ({parent, container}: {parent: HTMLElement; container: HTMLElement}): Promise<Node> =>
  new Promise<Node>((resolve) => {
    const text: Node = document.createTextNode('');

    const changeObserver: MutationObserver = new MutationObserver(() => {
      changeObserver.disconnect();

      resolve(text);
    });

    changeObserver.observe(container, {childList: true, subtree: true});

    parent.prepend(text);
  });

const updateNodeValue = ({
  container,
  oldValue,
  text
}: {
  oldValue: string;
  text: Node;
  container: HTMLElement;
}): Promise<{previousValue: string}> =>
  new Promise<{previousValue: string}>((resolve) => {
    const previousValue: string = text.nodeValue;

    const changeObserver: MutationObserver = new MutationObserver(() => {
      changeObserver.disconnect();

      resolve({previousValue});
    });

    changeObserver.observe(container, {characterData: true, subtree: true});

    text.nodeValue = oldValue;
  });

const createLast = ({container, paragraph}: {container: HTMLElement; paragraph: HTMLElement}): Promise<HTMLElement> =>
  new Promise<HTMLElement>((resolve) => {
    const anchor: HTMLElement = (paragraph.lastElementChild as HTMLElement) || document.createElement('span');

    const parent: HTMLElement = anchor.cloneNode() as HTMLElement;
    parent.innerHTML = '';

    const changeObserver: MutationObserver = new MutationObserver(() => {
      changeObserver.disconnect();

      resolve(parent);
    });

    changeObserver.observe(container, {childList: true, subtree: true});

    anchor.after(parent);
  });
