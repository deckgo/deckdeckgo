import {caretPosition, debounce} from '@deckdeckgo/utils';
import {nodeIndex} from '@deckdeckgo/editor';

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

export class DocUndoRedoEvents {
  private containerRef: HTMLElement;

  private dataObserver: MutationObserver | undefined;
  private treeObserver: MutationObserver | undefined;

  private undoValue: {mutation: MutationRecord; caretPosition: number} | undefined;
  private undoElements: {outerHTML: string; index: number}[];

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
  }

  destroy() {
    this.disconnect();

    this.disconnectInlineEditor();

    document.removeEventListener('keydown', this.onKeydown);
    document.removeEventListener('toolbarActivated', this.onInlineEditor);
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

  private stackUndoInput() {
    if (!this.undoValue) {
      return;
    }

    stackUndoInput(this.undoValue);

    this.undoValue = undefined;
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
    if (!this.undoValue) {
      const mutation: MutationRecord = mutations[0];

      const newValue: string = mutation.target.nodeValue;

      this.undoValue = {
        mutation,
        caretPosition: caretPosition({target: mutation.target}) + (mutation.oldValue.length > newValue.length ? 1 : -1)
      };
    }

    this.debounceUpdateInput();
  };

  private onTreeMutation = (mutations: MutationRecord[]) => {
    // New paragraph
    const addedParagraphs: HTMLElement[] = findAddedParagraphs({mutations, container: this.containerRef});
    addedParagraphs.forEach((paragraph: HTMLElement) =>
      stackUndoParagraph({
        paragraph,
        mutation: 'add',
        container: this.containerRef,
        previousSibling: paragraph.previousSibling as HTMLElement
      })
    );

    // Paragraphs removed
    const removedParagraphs: RemovedParagraph[] = findRemovedParagraphs({mutations});
    removedParagraphs.forEach(({paragraph, previousSibling}: RemovedParagraph) =>
      stackUndoParagraph({paragraph, mutation: 'remove', container: this.containerRef, previousSibling: previousSibling as HTMLElement})
    );
  };

  private onInlineEditorMutation = () => {
    if (this.undoElements.length > 0) {
      stackUndoUpdate({paragraphs: this.undoElements, container: this.containerRef});
    }

    this.copySelectedParagraphs();
  };

  // Copy current paragraphs value to a local state so we can add it to the undo redo global store in case of modifications
  private copySelectedParagraphs() {
    const paragraphs: HTMLElement[] = findSelectionParagraphs({container: this.containerRef});

    this.undoElements = paragraphs.map((paragraph: HTMLElement) => ({
      outerHTML: paragraph.outerHTML,
      index: nodeIndex(paragraph)
    }));
  }
}
