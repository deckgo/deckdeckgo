import {caretPosition, debounce} from '@deckdeckgo/utils';
import {nodeIndex} from '@deckdeckgo/editor';

import {UndoRedoDocInput, UndoRedoDocUpdateParagraph} from '../../../types/editor/undo-redo';

import {
  nextRedoChange,
  nextUndoChange,
  redo,
  stackUndoInput,
  stackUndoParagraph,
  stackUndoUpdate,
  undo
} from '../../../utils/editor/undo-redo.doc.utils';
import {
  findAddedParagraphs,
  RemovedParagraph,
  findRemovedParagraphs,
  findSelectionParagraphs,
  findAddedNodesParagraphs,
  findRemovedNodesParagraphs
} from '../../../utils/editor/paragraphs.utils';
import {findParagraph} from '../../../utils/editor/paragraph.utils';
import {NodeUtils} from '../../../utils/editor/node.utils';
import {SlotType} from '../../../types/editor/slot-type';

interface UndoUpdateParagraphs extends UndoRedoDocUpdateParagraph {
  paragraph: HTMLElement;
}

export class DocUndoRedoEvents {
  private containerRef: HTMLElement;

  private dataObserver: MutationObserver | undefined;
  private treeObserver: MutationObserver | undefined;
  private updateObserver: MutationObserver | undefined;

  private undoInput: UndoRedoDocInput | undefined = undefined;
  private undoUpdateParagraphs: UndoUpdateParagraphs[] = [];

  private readonly debounceUpdateInput: () => void = debounce(() => this.stackUndoInput(), 350);

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    this.undoInput = undefined;
    this.undoUpdateParagraphs = [];

    this.dataObserver = new MutationObserver(this.onDataMutation);
    this.treeObserver = new MutationObserver(this.onTreeMutation);
    this.updateObserver = new MutationObserver(this.onUpdateMutation);

    this.observe();

    document.addEventListener('keydown', this.onKeydown);
    document.addEventListener('toolbarActivated', this.onSelectionChange);
    document.addEventListener('focusin', this.onFocusIn);
  }

  destroy() {
    this.disconnect();

    document.removeEventListener('keydown', this.onKeydown);
    document.removeEventListener('toolbarActivated', this.onSelectionChange);
    document.removeEventListener('focusin', this.onFocusIn);
    document.removeEventListener('focusout', this.onFocusOut);
  }

  private observeKeydown() {
    document.addEventListener('keydown', this.onKeydown);
  }

  private disconnectKeydown() {
    document.removeEventListener('keydown', this.onKeydown);
  }

  private onKeydown = async ($event: KeyboardEvent) => {
    const {key, ctrlKey, metaKey, shiftKey} = $event;

    if (key === 'Enter') {
      this.stackUndoInput();
      return;
    }

    if (key === 'z' && (ctrlKey || metaKey) && !shiftKey) {
      await this.undo($event);
      return;
    }

    if (key === 'z' && (ctrlKey || metaKey) && shiftKey) {
      await this.redo($event);
      return;
    }
  };

  private async undo($event: KeyboardEvent) {
    $event.preventDefault();

    if (nextUndoChange() === undefined) {
      return;
    }

    await this.undoRedo({undoRedo: undo});
  }

  private async redo($event: KeyboardEvent) {
    $event.preventDefault();

    if (nextRedoChange() === undefined) {
      return;
    }

    await this.undoRedo({undoRedo: redo});
  }

  private onFocusIn = ({target}: FocusEvent) => {
    const focusedElement: HTMLElement | undefined | null = NodeUtils.toHTMLElement(target as Node);

    if (!focusedElement || focusedElement.nodeName.toLowerCase() !== SlotType.CODE) {
      return;
    }

    // We use the browser capability when editing a code block and once done, we stack in the custom undo-redo store the all modification
    this.disconnectKeydown();
    this.disconnect();

    document.addEventListener('focusout', this.onFocusOut, {once: true});

    this.undoUpdateParagraphs = [
      {
        outerHTML: focusedElement.outerHTML,
        index: nodeIndex(focusedElement),
        paragraph: focusedElement
      }
    ];
  };

  private onFocusOut = ({target}: FocusEvent) => {
    // Should not happen
    if (this.undoUpdateParagraphs.length <= 0) {
      this.observeKeydown();
      this.observe();
      return;
    }

    const focusedElement: HTMLElement | undefined | null = NodeUtils.toHTMLElement(target as Node);

    // Should not happen neither
    if (!focusedElement || focusedElement.nodeName.toLowerCase() !== SlotType.CODE) {
      this.observeKeydown();
      this.observe();
      return;
    }

    if (focusedElement.outerHTML === this.undoUpdateParagraphs[0].outerHTML) {
      this.observeKeydown();
      this.observe();
      return;
    }

    stackUndoUpdate({paragraphs: this.undoUpdateParagraphs, container: this.containerRef});

    this.observeKeydown();
    this.observe();
  };

  private stackUndoInput() {
    if (!this.undoInput || this.undoUpdateParagraphs.length > 0) {
      return;
    }

    stackUndoInput({
      data: this.undoInput,
      container: this.containerRef
    });

    this.undoInput = undefined;
  }

  private async undoRedo({undoRedo}: {undoRedo: () => Promise<void>}) {
    // Undo and redo the input will trigger the MutationObserver therefore we disable it and observe next change to add it again
    // In other words, we skip one mutation change event
    this.disconnect();

    const changeObserver: MutationObserver = new MutationObserver(() => {
      changeObserver.disconnect();

      this.observe();
    });

    changeObserver.observe(this.containerRef, {characterData: true, subtree: true});

    await undoRedo();
  }

  private observe() {
    this.treeObserver.observe(this.containerRef, {childList: true, subtree: true});
    this.dataObserver.observe(this.containerRef, {characterData: true, subtree: true, characterDataOldValue: true});
    this.updateObserver.observe(this.containerRef, {childList: true, subtree: true, attributes: true});
  }

  private disconnect() {
    this.treeObserver.disconnect();
    this.dataObserver.disconnect();
    this.updateObserver.disconnect();
  }

  private onSelectionChange = () => {
    this.copySelectedParagraphs();
  };

  // Copy current paragraphs value to a local state so we can add it to the undo redo global store in case of modifications
  private copySelectedParagraphs() {
    const paragraphs: HTMLElement[] = findSelectionParagraphs({container: this.containerRef});

    this.undoUpdateParagraphs = paragraphs.map((paragraph: HTMLElement) => ({
      outerHTML: paragraph.outerHTML,
      index: nodeIndex(paragraph),
      paragraph
    }));
  }

  private onDataMutation = (mutations: MutationRecord[]) => {
    if (!this.undoInput) {
      const mutation: MutationRecord = mutations[0];

      const target: Node = mutation.target;

      const newValue: string = target.nodeValue;

      const paragraph: HTMLElement | undefined = NodeUtils.toHTMLElement(findParagraph({element: target, container: this.containerRef}));

      if (!paragraph || !target.parentNode) {
        return;
      }

      // We find the list of node indexes of the parent of the modified text
      const depths: number[] = [];

      let parentElement: HTMLElement = target.parentElement;
      while (!parentElement.isSameNode(paragraph)) {
        depths.push(nodeIndex(parentElement));
        parentElement = parentElement.parentElement;
      }

      this.undoInput = {
        oldValue: mutation.oldValue,
        offset: caretPosition({target}) + (mutation.oldValue.length - newValue.length),
        index: nodeIndex(paragraph),
        indexDepths: depths
      };
    }

    this.debounceUpdateInput();
  };

  private onTreeMutation = (mutations: MutationRecord[]) => {
    const changes: {paragraph: HTMLElement; index: number; mutation: 'remove' | 'add'}[] = [];

    // New paragraph
    const addedParagraphs: HTMLElement[] = findAddedParagraphs({mutations, container: this.containerRef});
    addedParagraphs.forEach((paragraph: HTMLElement) =>
      changes.push({
        paragraph,
        mutation: 'add',
        index: paragraph.previousSibling ? nodeIndex(NodeUtils.toHTMLElement(paragraph.previousSibling)) + 1 : 0
      })
    );

    // Paragraphs removed
    const removedParagraphs: RemovedParagraph[] = findRemovedParagraphs({mutations});

    const lowerIndex: number = Math.min(
      ...removedParagraphs.map(({previousSibling}: RemovedParagraph) =>
        previousSibling ? nodeIndex(NodeUtils.toHTMLElement(previousSibling)) + 1 : 0
      )
    );

    removedParagraphs.forEach(({paragraph}: RemovedParagraph, index: number) =>
      changes.push({paragraph, mutation: 'remove', index: index + lowerIndex})
    );

    if (changes.length <= 0) {
      return;
    }

    stackUndoParagraph({
      container: this.containerRef,
      changes
    });
  };

  private onUpdateMutation = (mutations: MutationRecord[]) => {
    const addedNodesMutations: MutationRecord[] = findAddedNodesParagraphs({mutations, container: this.containerRef});
    const removedNodesMutations: MutationRecord[] = findRemovedNodesParagraphs({mutations, container: this.containerRef});

    const needsUpdate: boolean = addedNodesMutations.length > 0 || removedNodesMutations.length > 0;

    if (!needsUpdate) {
      return;
    }

    if (this.undoUpdateParagraphs.length <= 0) {
      return;
    }

    stackUndoUpdate({
      paragraphs: this.undoUpdateParagraphs.filter(({paragraph}: UndoUpdateParagraphs) => paragraph.isConnected),
      container: this.containerRef
    });

    this.undoUpdateParagraphs = [];
  };
}
