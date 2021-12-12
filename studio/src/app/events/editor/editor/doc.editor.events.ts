import {catchTab} from '@deckdeckgo/utils';

import {attachPasteEvent, detachPasteEvent} from '../../../utils/editor/paste.utils';

export class DocEditorEvents {
  init() {
    attachPasteEvent();

    document.addEventListener('keydown', ($event: KeyboardEvent) => catchTab($event));
  }

  destroy() {
    detachPasteEvent();

    document.removeEventListener('keydown', ($event: KeyboardEvent) => catchTab($event));
  }
}
