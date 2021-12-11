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
  findSelectionParagraphs
} from '../../../utils/editor/paragraphs.utils';
import {findParagraph} from '../../../utils/editor/paragraph.utils';
import {NodeUtils} from '../../../utils/editor/node.utils';
import {SlotType} from '../../../types/editor/slot-type';

export class DocUndoRedoEvents {
  private containerRef: HTMLElement;

  private dataObserver: MutationObserver | undefined;
  private treeObserver: MutationObserver | undefined;

  private undoInput: UndoRedoDocInput | undefined;
  private undoUpdateParagraphs: UndoRedoDocUpdateParagraph[] = [];

  private inlineEditorObserver: MutationObserver | undefined;

  private readonly debounceUpdateInput: () => void = debounce(() => this.stackUndoInput(), 350);

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    this.dataObserver = new MutationObserver(this.onDataMutation);
    this.treeObserver = new MutationObserver(this.onTreeMutation);

    this.inlineEditorObserver = new MutationObserver(this.onInlineEditorMutation);

    this.observe();

    document.addEventListener('keydown', this.onKeydown);
    document.addEventListener('toolbarActivated', this.onInlineEditor);
    document.addEventListener('focusin', this.onFocusIn);
  }

  destroy() {
    this.disconnect();

    this.disconnectInlineEditor();

    document.removeEventListener('keydown', this.onKeydown);
    document.removeEventListener('toolbarActivated', this.onInlineEditor);
    document.removeEventListener('focusin', this.onFocusIn);
    document.removeEventListener('focusout', this.onFocusOut);
  }

  private observeKeydown() {
    document.addEventListener('keydown', this.onKeydown);
  }

  private disconnectKeydown() {
    document.removeEventListener('keydown', this.onKeydown);
  }

  private onKeydown = ($event: KeyboardEvent) => {
    const {key, ctrlKey, metaKey, shiftKey} = $event;

    if (key === 'Enter') {
      this.stackUndoInput();
      return;
    }

    if (key === 'z' && (ctrlKey || metaKey) && !shiftKey) {
      this.undo($event);
      return;
    }

    if (key === 'z' && (ctrlKey || metaKey) && shiftKey) {
      this.redo($event);
      return;
    }
  };

  private undo($event: KeyboardEvent) {
    if (nextUndoChange() === undefined) {
      return;
    }

    this.undoRedo({$event, undoRedo: undo});
  }

  private redo($event: KeyboardEvent) {
    if (nextRedoChange() === undefined) {
      return;
    }

    this.undoRedo({$event, undoRedo: redo});
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
        index: nodeIndex(focusedElement)
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
    if (!this.undoInput) {
      return;
    }

    stackUndoInput({
      data: this.undoInput,
      container: this.containerRef
    });

    this.undoInput = undefined;
  }

  private undoRedo({$event, undoRedo}: {$event: KeyboardEvent; undoRedo: () => void}) {
    $event.preventDefault();

    // Undo and redo the input will trigger the MutationObserver therefore we disable it and observe next change to add it again
    // In other words, we skip one mutation change event
    this.disconnect();

    const changeObserver: MutationObserver = new MutationObserver(() => {
      changeObserver.disconnect();

      this.observe();
    });

    changeObserver.observe(this.containerRef, {characterData: true, subtree: true});

    undoRedo();
  }

  private observe() {
    this.treeObserver.observe(this.containerRef, {childList: true, subtree: true});
    this.dataObserver.observe(this.containerRef, {characterData: true, subtree: true, characterDataOldValue: true});
  }

  private disconnect() {
    this.treeObserver.disconnect();
    this.dataObserver.disconnect();
  }

  private observeInlineEditor() {
    this.inlineEditorObserver.observe(this.containerRef, {childList: true, subtree: true, attributes: true});
  }

  private disconnectInlineEditor() {
    this.inlineEditorObserver.disconnect();
  }

  private onInlineEditor = ({detail}: CustomEvent<boolean>) => {
    if (!detail) {
      this.disconnectInlineEditor();

      this.observe();

      return;
    }

    this.disconnect();

    this.observeInlineEditor();

    this.copySelectedParagraphs();
  };

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
        offset: caretPosition({target}) + (mutation.oldValue.length > newValue.length ? 1 : -1),
        index: nodeIndex(paragraph),
        indexDepths: depths
      };
    }

    this.debounceUpdateInput();
  };

  private onTreeMutation = (mutations: MutationRecord[]) => {
    const changes: {paragraph: HTMLElement; previousSibling?: HTMLElement; mutation: 'remove' | 'add'}[] = [];

    // New paragraph
    const addedParagraphs: HTMLElement[] = findAddedParagraphs({mutations, container: this.containerRef});
    addedParagraphs.forEach((paragraph: HTMLElement) =>
      changes.push({
        paragraph,
        mutation: 'add',
        previousSibling: paragraph.previousSibling as HTMLElement
      })
    );

    // Paragraphs removed
    const removedParagraphs: RemovedParagraph[] = findRemovedParagraphs({mutations});
    removedParagraphs.forEach(({paragraph, previousSibling}: RemovedParagraph) =>
      changes.push({paragraph, mutation: 'remove', previousSibling: previousSibling as HTMLElement})
    );

    if (changes.length <= 0) {
      return;
    }

    stackUndoParagraph({
      container: this.containerRef,
      changes
    });
  };

  private onInlineEditorMutation = () => {
    if (this.undoUpdateParagraphs.length > 0) {
      stackUndoUpdate({paragraphs: this.undoUpdateParagraphs, container: this.containerRef});
    }

    this.copySelectedParagraphs();
  };

  // Copy current paragraphs value to a local state so we can add it to the undo redo global store in case of modifications
  private copySelectedParagraphs() {
    const paragraphs: HTMLElement[] = findSelectionParagraphs({container: this.containerRef});

    this.undoUpdateParagraphs = paragraphs.map((paragraph: HTMLElement) => ({
      outerHTML: paragraph.outerHTML,
      index: nodeIndex(paragraph)
    }));
  }
}
