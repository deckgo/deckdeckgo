import {caretPosition, debounce} from '@deckdeckgo/utils';

import {nextRedoChange, nextUndoChange, redo, stackUndoInput, undo} from '../../../utils/editor/undo-redo.doc.utils';

export class DocUndoRedoEvents {
  private containerRef: HTMLElement;

  private dataObserver: MutationObserver | undefined;

  private undoValue: {mutation: MutationRecord; caretPosition: number} | undefined;

  private readonly debounceUpdateInput: () => void = debounce(() => this.stackUndoInput(), 350);

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    this.dataObserver = new MutationObserver(this.onDataMutation);
    this.observeInput();

    document.addEventListener('keydown', this.onKeydown);
  }

  destroy() {
    this.dataObserver?.disconnect();

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
    this.disconnectInput();

    const changeObserver: MutationObserver = new MutationObserver(() => {
      changeObserver.disconnect();

      this.observeInput();
    });

    changeObserver.observe(this.containerRef, {characterData: true, subtree: true});

    undoRedo();
  }

  private observeInput() {
    this.dataObserver.observe(this.containerRef, {characterData: true, subtree: true, characterDataOldValue: true});
  }

  private disconnectInput() {
    this.dataObserver.disconnect();
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
}
