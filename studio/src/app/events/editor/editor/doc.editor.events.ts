import {catchTab} from '@deckdeckgo/utils';

import {attachPasteEvent, detachPasteEvent} from '../../../utils/editor/paste.utils';

export class DocEditorEvents {
  private containerRef: HTMLElement;

  init(containerRef: HTMLElement) {
    this.containerRef = containerRef;

    attachPasteEvent();

    document.addEventListener('keydown', this.catchTab);
  }

  destroy() {
    detachPasteEvent();

    document.removeEventListener('keydown', this.catchTab);
  }

  private catchTab = ($event: KeyboardEvent) => {
    if (!this.containerRef?.isEqualNode($event.target as Node)) {
      return;
    }

    catchTab($event);
  };
}
