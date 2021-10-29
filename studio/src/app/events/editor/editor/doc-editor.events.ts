import {attachPasteEvent, detachPasteEvent} from '../../../utils/editor/paste.utils';
import {catchTab} from '@deckdeckgo/utils';

export class DocEditorEvents {
  init() {
    attachPasteEvent();

    document.addEventListener('keydown', ($event: KeyboardEvent) => catchTab($event), false);
  }

  destroy() {
    detachPasteEvent();

    document.removeEventListener('keydown', ($event: KeyboardEvent) => catchTab($event), true);
  }
}
