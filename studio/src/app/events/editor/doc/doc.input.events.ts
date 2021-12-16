import {moveCursorToEnd} from '@deckdeckgo/utils';

export class DocInputEvents {
  private containerRef: HTMLElement;

  private inputObserver: MutationObserver | undefined;

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    this.inputObserver = new MutationObserver(this.onCharacterDataMutation);

    this.observe();
  }

  destroy() {
    this.disconnect();
  }

  private observe() {
    this.inputObserver.observe(this.containerRef, {characterData: true, subtree: true, characterDataOldValue: true});
  }

  private disconnect() {
    this.inputObserver.disconnect();
  }

  // TODO: proof of concept

  private onCharacterDataMutation = (mutations: MutationRecord[]) => {
    console.log(mutations[0].oldValue, mutations[0].target.nodeValue);

    if (mutations[0].oldValue.charAt(mutations[0].oldValue.length - 1) === '*') {
      // TODO delay for undo redo or ignore?
      mutations[0].target.nodeValue = mutations[0].target.nodeValue.substring(0, mutations[0].target.nodeValue.length - 2) + ' ';

      const span = document.createElement('span');
      span.innerHTML = '\u200B';
      span.style.fontWeight = '700';

      const changeObserver: MutationObserver = new MutationObserver((mutations: MutationRecord[]) => {
        changeObserver.disconnect();

        console.log(mutations);

        moveCursorToEnd(mutations[0].addedNodes[0]);
      });

      changeObserver.observe(this.containerRef, {childList: true, subtree: true});

      if (mutations[0].target.nextSibling) {
        mutations[0].target.parentNode.insertBefore(span, mutations[0].target.nextSibling);
      } else {
        mutations[0].target.parentNode.appendChild(span);
      }
    }
  };
}
