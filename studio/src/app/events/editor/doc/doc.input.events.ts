import {caretPosition, getSelection, moveCursorToEnd} from '@deckdeckgo/utils';

import undoRedoStore from '../../../stores/undo-redo.store';

import {NodeUtils} from '../../../utils/editor/node.utils';

export class DocInputEvents {
  private containerRef: HTMLElement;

  private lastKey: string | undefined = undefined;

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    document.addEventListener('keydown', this.catchTab);
  }

  destroy() {
    document.removeEventListener('keydown', this.catchTab);
  }

  private catchTab = async ($event: KeyboardEvent) => {
    const {key} = $event;

    if (this.lastKey === '*' && key === '*') {
      await this.switchToBold($event);

      this.lastKey = undefined;
      return;
    }

    this.lastKey = key;
  };

  private async switchToBold($event: KeyboardEvent) {
    const selection: Selection | null = getSelection();

    if (!selection) {
      return;
    }

    $event.preventDefault();

    // Disable undo-redo observer as we are about to play with the DOM
    undoRedoStore.state.observe = false;

    const target: Node = selection.focusNode;

    const parent: HTMLElement = NodeUtils.toHTMLElement(target);

    // We eiter remove the last character, a *, or split the text around the selection and *
    await this.updateText({target, parent});

    // We had fun, we can observe again the undo redo store to stack the next bold element we are about to create
    undoRedoStore.state.observe = true;

    await this.createNode({target, parent});
  }

  private createNode({target, parent}: {target: Node; parent: HTMLElement}): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const changeObserver: MutationObserver = new MutationObserver((mutations: MutationRecord[]) => {
        changeObserver.disconnect();

        moveCursorToEnd(mutations[0]?.addedNodes[0]);

        resolve();
      });

      changeObserver.observe(this.containerRef, {childList: true, subtree: true});

      const {fontWeight}: CSSStyleDeclaration = window.getComputedStyle(parent);

      const isBold: boolean = parseInt(fontWeight) > 400 || fontWeight === 'bold';

      if (isBold) {
        // We are in a bold node, therefore we want to exit it
        const newText: Text = document.createTextNode('\u200B');
        parent.after(newText);
      } else {
        // We create the bold node
        const span: HTMLSpanElement = document.createElement('span');
        span.innerHTML = '\u200B';
        span.style.fontWeight = 'bold';

        if (target.nextSibling) {
          parent.insertBefore(span, target.nextSibling);
        } else {
          parent.appendChild(span);
        }
      }
    });
  }

  private updateText({target, parent}: {target: Node; parent: HTMLElement}): Promise<void> {
    return new Promise<void>((resolve) => {
      const index: number = caretPosition({target});

      // Exact same length, so we remove the last character, the *
      if (target.nodeValue.length === index) {
        target.nodeValue = target.nodeValue.substring(0, target.nodeValue.length - 1);
        resolve();
        return;
      }

      // The end results will be text followed by a span bold and then the remaining text
      const newText: Text = (target as Text).splitText(index);

      const changeObserver: MutationObserver = new MutationObserver(() => {
        changeObserver.disconnect();

        resolve();
      });

      changeObserver.observe(this.containerRef, {childList: true, subtree: true});

      if (target.nextSibling) {
        parent.insertBefore(newText, target.nextSibling);
      } else {
        parent.appendChild(newText);
      }
    });
  }
}
