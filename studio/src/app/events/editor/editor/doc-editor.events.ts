import {attachPasteEvent, detachPasteEvent} from '../../../utils/editor/paste.utils';

export class DocEditorEvents {
  init() {
    attachPasteEvent();
  }

  destroy() {
    detachPasteEvent();
  }
}
