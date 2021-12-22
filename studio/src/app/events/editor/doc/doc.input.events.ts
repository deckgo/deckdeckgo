import {caretPosition, getSelection, moveCursorToEnd} from '@deckdeckgo/utils';

import undoRedoStore from '../../../stores/undo-redo.store';

import {NodeUtils} from '../../../utils/editor/node.utils';

interface Key {
  key: string;
}

interface TransformInput {
  match: ({lastKey, key}: {lastKey: Key | undefined; key: Key}) => boolean;
  transform: () => HTMLElement;
  postTransform?: () => Promise<void>;
  active: (parent: HTMLElement) => boolean;
  trim: () => number;
}

export class DocInputEvents {
  private containerRef: HTMLElement;

  private lastBeforeInput: Key | undefined = undefined;

  private beforeInputTransformer: TransformInput[] = [
    {
      match: ({lastKey, key}: {lastKey: Key | undefined; key: Key}) => lastKey?.key === '`' && key.key === '`',
      transform: (): HTMLElement => {
        return document.createElement('mark');
      },
      active: ({nodeName}: HTMLElement) => nodeName.toLowerCase() === 'mark',
      trim: (): number => '`'.length,
      postTransform: () => this.replaceBacktick()
    },
    {
      match: ({lastKey, key}: {lastKey: Key | undefined; key: Key}) => lastKey?.key === '*' && key.key === '*',
      transform: (): HTMLElement => {
        const span: HTMLSpanElement = document.createElement('span');
        span.style.fontWeight = 'bold';
        return span;
      },
      active: (parent: HTMLElement) => {
        const {fontWeight}: CSSStyleDeclaration = window.getComputedStyle(parent);

        return parseInt(fontWeight) > 400 || fontWeight === 'bold';
      },
      trim: (): number => '*'.length
    },
    {
      match: ({lastKey, key}: {lastKey: Key | undefined; key: Key}) => lastKey?.key === ' ' && key.key === '_',
      transform: (): HTMLElement => {
        const span: HTMLSpanElement = document.createElement('span');
        span.style.fontStyle = 'italic';
        return span;
      },
      active: (parent: HTMLElement) => {
        const {fontStyle}: CSSStyleDeclaration = window.getComputedStyle(parent);

        return fontStyle === 'italic';
      },
      trim: (): number => ''.length
    }
  ];

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    document.addEventListener('beforeinput', this.onBeforeInput);
  }

  destroy() {
    document.addEventListener('beforeinput', this.onBeforeInput);
  }

  private onBeforeInput = async ($event: InputEvent) => {
    const {data} = $event;

    const transformInput: TransformInput | undefined = this.beforeInputTransformer.find(({match}: TransformInput) =>
      match({key: {key: data}, lastKey: this.lastBeforeInput})
    );

    if (transformInput !== undefined) {
      await this.transform({$event, transformInput});

      await transformInput.postTransform?.();

      this.lastBeforeInput = undefined;
      return;
    }

    this.lastBeforeInput = {key: data};
  };

  private async transform({$event, transformInput}: {$event: KeyboardEvent | InputEvent; transformInput: TransformInput}) {
    const selection: Selection | null = getSelection();

    if (!selection) {
      return;
    }

    const {focusNode: target} = selection;

    if (!target) {
      return;
    }

    $event.preventDefault();

    // Disable undo-redo observer as we are about to play with the DOM
    undoRedoStore.state.observe = false;

    const parent: HTMLElement = NodeUtils.toHTMLElement(target);

    // Check if we can transform or end tag
    if (!this.canTransform({target, parent, transformInput})) {
      return;
    }

    // We eiter remove the last character, a *, or split the text around the selection and *
    await this.updateText({target, parent, transformInput});

    // We had fun, we can observe again the undo redo store to stack the next bold element we are about to create
    undoRedoStore.state.observe = true;

    await this.createNode({target, parent, transformInput});
  }

  private createNode({target, parent, transformInput}: {target: Node; parent: HTMLElement; transformInput: TransformInput}): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const changeObserver: MutationObserver = new MutationObserver((mutations: MutationRecord[]) => {
        changeObserver.disconnect();

        moveCursorToEnd(mutations[0]?.addedNodes[0]);

        resolve();
      });

      changeObserver.observe(this.containerRef, {childList: true, subtree: true});

      const {active, transform} = transformInput;

      if (active(parent)) {
        // We are in a bold node, therefore we want to exit it
        const newText: Text = document.createTextNode('\u200B');
        parent.after(newText);
      } else {
        // We create the new node
        const newNode: HTMLElement = transform();
        newNode.innerHTML = '\u200B';

        if (target.nextSibling) {
          parent.insertBefore(newNode, target.nextSibling);
        } else {
          parent.appendChild(newNode);
        }
      }
    });
  }

  private canTransform({target, parent, transformInput}: {target: Node; parent: HTMLElement; transformInput: TransformInput}): boolean {
    const index: number = caretPosition({target});

    // We are typing at the end of the node text, we can transform it
    if (target.nodeValue.length === index) {
      return true;
    }

    // We are typing in the middle of a text node, we can transform it or end it only if not yet transformed
    const {active} = transformInput;
    return !active(parent);
  }

  private updateText({target, parent, transformInput}: {target: Node; parent: HTMLElement; transformInput: TransformInput}): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const index: number = caretPosition({target});

      // Exact same length, so we remove the last characters
      if (target.nodeValue.length === index) {
        const changeObserver: MutationObserver = new MutationObserver(() => {
          changeObserver.disconnect();

          resolve();
        });

        changeObserver.observe(this.containerRef, {characterData: true, subtree: true});

        target.nodeValue = target.nodeValue.substring(0, target.nodeValue.length - transformInput.trim());

        return;
      }

      // The end results will be text followed by a span bold and then the remaining text
      const newText: Node = await this.splitText({target, index, transformInput});

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

  private splitText({target, index, transformInput}: {target: Node; index: number; transformInput: TransformInput}): Promise<Node> {
    return new Promise<Node>((resolve) => {
      const changeObserver: MutationObserver = new MutationObserver(async () => {
        changeObserver.disconnect();

        const node: Node = await this.removeChar({target: newText, index: 1});

        resolve(node);
      });

      changeObserver.observe(this.containerRef, {childList: true, subtree: true});

      const newText: Text = (target as Text).splitText(index - transformInput.trim());
    });
  }

  private removeChar({target, index}: {target: Node; index: number}): Promise<Node> {
    return new Promise<Node>((resolve) => {
      const changeObserver: MutationObserver = new MutationObserver((mutations: MutationRecord[]) => {
        changeObserver.disconnect();

        resolve(mutations[0].target);
      });

      changeObserver.observe(this.containerRef, {characterData: true, subtree: true});

      target.nodeValue = target.nodeValue.slice(index);
    });
  }

  /**
   * Browser render the backtick afterwards therefore we have to delete it
   */
  private replaceBacktick(): Promise<void> {
    return new Promise<void>((resolve) => {
      const changeObserver: MutationObserver = new MutationObserver(async (mutation: MutationRecord[]) => {
        changeObserver.disconnect();

        const target: Node = mutation[0].target;

        undoRedoStore.state.observe = false;

        await this.replaceChar({target, searchValue: '`', replaceValue: ''});

        undoRedoStore.state.observe = true;

        moveCursorToEnd(target);

        resolve();
      });

      changeObserver.observe(this.containerRef, {characterData: true, subtree: true});
    });
  }

  private replaceChar({target, searchValue, replaceValue}: {target: Node; searchValue: string; replaceValue: string}): Promise<Node> {
    return new Promise<Node>((resolve) => {
      const changeObserver: MutationObserver = new MutationObserver((mutations: MutationRecord[]) => {
        changeObserver.disconnect();

        resolve(mutations[0].target);
      });

      changeObserver.observe(this.containerRef, {characterData: true, subtree: true});

      target.nodeValue = target.nodeValue.replace(searchValue, replaceValue);
    });
  }
}
