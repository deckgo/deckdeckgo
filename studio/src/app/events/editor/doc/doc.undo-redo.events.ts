import {caretPosition, debounce} from '@deckdeckgo/utils';

import {nextRedoChange, nextUndoChange, redo, stackUndoInput, stackUndoParagraph, undo} from '../../../utils/editor/undo-redo.doc.utils';
import {findAddedParagraphs, RemovedParagraph, findRemovedParagraphs} from '../../../utils/editor/paragraphs.utils';

export class DocUndoRedoEvents {
  private containerRef: HTMLElement;

  private dataObserver: MutationObserver | undefined;
  private treeObserver: MutationObserver | undefined;

  private undoValue: {mutation: MutationRecord; caretPosition: number} | undefined;

  private readonly debounceUpdateInput: () => void = debounce(() => this.stackUndoInput(), 350);

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    this.dataObserver = new MutationObserver(this.onDataMutation);
    this.treeObserver = new MutationObserver(this.onTreeMutation);

    this.observe();

    document.addEventListener('keydown', this.onKeydown);
  }

  destroy() {
    this.disconnect();

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
    this.dataObserver.observe(this.containerRef, {characterData: true, subtree: true, characterDataOldValue: true});
    this.treeObserver.observe(this.containerRef, {childList: true, subtree: true});
  }

  private disconnect() {
    this.dataObserver.disconnect();
    this.treeObserver.disconnect();
  }

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
    const addedParagraphs: HTMLElement[] = findAddedParagraphs({mutations, container: this.containerRef});
    addedParagraphs.forEach((paragraph: HTMLElement) =>
      stackUndoParagraph({
        paragraph,
        mutation: 'add',
        container: this.containerRef,
        previousSibling: paragraph.previousSibling as HTMLElement
      })
    );

    const removedParagraphs: RemovedParagraph[] = findRemovedParagraphs({mutations});
    removedParagraphs.forEach(({paragraph, previousSibling}: RemovedParagraph) =>
      stackUndoParagraph({paragraph, mutation: 'remove', container: this.containerRef, previousSibling: previousSibling as HTMLElement})
    );
  };
}
