import {caretPosition, debounce} from '@deckdeckgo/utils';
import {elementIndex, nodeIndex} from '@deckdeckgo/editor';

import undoRedoStore from '../../../stores/undo-redo.store';

import {UndoRedoDocAddRemoveParagraph, UndoRedoDocInput, UndoRedoDocUpdateParagraph} from '../../../types/editor/undo-redo';

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
  findRemovedNodesParagraphs,
  findUpdatedParagraphs
} from '../../../utils/editor/paragraphs.utils';
import {findParagraph} from '../../../utils/editor/paragraph.utils';
import {NodeUtils} from '../../../utils/editor/node.utils';

import {SlotType} from '../../../types/editor/slot-type';

interface UndoUpdateParagraphs extends UndoRedoDocUpdateParagraph {
  paragraph: HTMLElement;
}

export class DocUndoRedoEvents {
  private containerRef: HTMLElement;

  private inputObserver: MutationObserver | undefined;
  private treeObserver: MutationObserver | undefined;
  private updateObserver: MutationObserver | undefined;
  private attributesObserver: MutationObserver | undefined;

  private undoInput: UndoRedoDocInput | undefined = undefined;
  private undoUpdateParagraphs: UndoUpdateParagraphs[] = [];

  private readonly debounceUpdateInput: () => void = debounce(() => this.stackUndoInput(), 350);

  private unsubscribe;

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    this.undoInput = undefined;
    this.undoUpdateParagraphs = [];

    this.inputObserver = new MutationObserver(this.onCharacterDataMutation);
    this.treeObserver = new MutationObserver(this.onTreeMutation);
    this.updateObserver = new MutationObserver(this.onUpdateMutation);
    this.attributesObserver = new MutationObserver(this.onAttributesMutation);

    this.observe();

    document.addEventListener('keydown', this.onKeydown);
    document.addEventListener('toolbarActivated', this.onSelectionChange);
    document.addEventListener('focusin', this.onFocusIn);

    this.unsubscribe = undoRedoStore.onChange('observe', (observe: boolean) => {
      if (observe) {
        // We re-active the selection as if we would have selected a paragraphs because we might need to record next update
        this.copySelectedParagraphs({filterEmptySelection: false});
        this.undoInput = undefined;

        this.observe();
        return;
      }

      this.disconnect();
    });
  }

  destroy() {
    this.disconnect();

    document.removeEventListener('keydown', this.onKeydown);
    document.removeEventListener('toolbarActivated', this.onSelectionChange);
    document.removeEventListener('focusin', this.onFocusIn);
    document.removeEventListener('focusout', this.onFocusOut);

    this.unsubscribe?.();
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
        index: elementIndex(focusedElement),
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
    this.inputObserver.observe(this.containerRef, {characterData: true, subtree: true, characterDataOldValue: true});
    this.updateObserver.observe(this.containerRef, {childList: true, subtree: true});
    this.attributesObserver.observe(this.containerRef, {attributes: true, subtree: true});
  }

  private disconnect() {
    this.treeObserver.disconnect();
    this.inputObserver.disconnect();
    this.updateObserver.disconnect();
    this.attributesObserver.disconnect();
  }

  private onSelectionChange = () => {
    this.copySelectedParagraphs({filterEmptySelection: true});
  };

  // Copy current paragraphs value to a local state so we can add it to the undo redo global store in case of modifications
  private copySelectedParagraphs({filterEmptySelection}: {filterEmptySelection: boolean}) {
    const paragraphs: HTMLElement[] | undefined = findSelectionParagraphs({container: this.containerRef, filterEmptySelection});

    if (!paragraphs) {
      return;
    }

    this.undoUpdateParagraphs = this.toUpdateParagraphs(paragraphs);
  }

  private toUpdateParagraphs(paragraphs: HTMLElement[]): UndoUpdateParagraphs[] {
    return paragraphs.map((paragraph: HTMLElement) => ({
      outerHTML: paragraph.outerHTML,
      index: elementIndex(paragraph),
      paragraph
    }));
  }

  private onCharacterDataMutation = (mutations: MutationRecord[]) => {
    if (!this.undoInput) {
      const mutation: MutationRecord = mutations[0];

      const target: Node = mutation.target;

      const newValue: string = target.nodeValue;

      const paragraph: HTMLElement | undefined = NodeUtils.toHTMLElement(findParagraph({element: target, container: this.containerRef}));

      if (!paragraph || !target.parentNode) {
        return;
      }

      // We find the list of node indexes of the parent of the modified text
      const depths: number[] = [nodeIndex(target)];

      let parentElement: HTMLElement = target.parentElement;
      while (!parentElement.isSameNode(paragraph)) {
        depths.push(nodeIndex(parentElement));
        parentElement = parentElement.parentElement;
      }

      this.undoInput = {
        oldValue: mutation.oldValue,
        offset: caretPosition({target}) + (mutation.oldValue.length - newValue.length),
        index: elementIndex(paragraph),
        indexDepths: depths.reverse()
      };
    }

    this.debounceUpdateInput();
  };

  private onTreeMutation = (mutations: MutationRecord[]) => {
    const changes: UndoRedoDocAddRemoveParagraph[] = [];

    // New paragraph
    const addedParagraphs: HTMLElement[] = findAddedParagraphs({mutations, container: this.containerRef});
    addedParagraphs.forEach((paragraph: HTMLElement) =>
      changes.push({
        outerHTML: this.cleanOuterHTML(paragraph),
        mutation: 'add',
        index: paragraph.previousSibling ? elementIndex(NodeUtils.toHTMLElement(paragraph.previousSibling)) + 1 : 0
      })
    );

    const addedIndex: number[] = changes.map(({index}: UndoRedoDocAddRemoveParagraph) => index);

    // Paragraphs removed
    const removedParagraphs: RemovedParagraph[] = findRemovedParagraphs({mutations});

    const lowerIndex: number = Math.min(
      ...removedParagraphs.map(({previousSibling}: RemovedParagraph) =>
        previousSibling ? elementIndex(NodeUtils.toHTMLElement(previousSibling)) + 1 : 0
      )
    );

    removedParagraphs
      .filter((_removedParagraph: RemovedParagraph, index: number) => !addedIndex.includes(index + lowerIndex))
      .forEach(({paragraph}: RemovedParagraph, index: number) =>
        changes.push({outerHTML: this.cleanOuterHTML(paragraph), mutation: 'remove', index: index + lowerIndex})
      );

    if (changes.length <= 0) {
      return;
    }

    stackUndoParagraph({
      container: this.containerRef,
      changes
    });
  };

  // We do not want to overwrite the auto generated paragraph_id
  private cleanOuterHTML(paragraph: HTMLElement): string {
    const clone: HTMLElement = paragraph.cloneNode(true) as HTMLElement;
    clone.removeAttribute('paragraph_id');
    return clone.outerHTML;
  }

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

    this.copySelectedParagraphs({filterEmptySelection: true});
  };

  private onAttributesMutation = async (mutations: MutationRecord[]) => {
    const updateParagraphs: HTMLElement[] = findUpdatedParagraphs({
      mutations: mutations.filter(({attributeName}: MutationRecord) => ['style'].includes(attributeName)),
      container: this.containerRef
    });

    if (updateParagraphs.length <= 0) {
      return;
    }

    stackUndoUpdate({
      paragraphs: this.undoUpdateParagraphs,
      container: this.containerRef
    });

    this.copySelectedParagraphs({filterEmptySelection: false});
  };
}
