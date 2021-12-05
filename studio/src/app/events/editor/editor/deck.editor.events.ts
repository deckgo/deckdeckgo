import {redo, undo} from '../../../utils/editor/undo-redo.utils';
import {attachPasteEvent, detachPasteEvent} from '../../../utils/editor/paste.utils';

export class DeckEditorEvents {
  private mainRef: HTMLElement;
  private actionsEditorRef: HTMLAppActionsDeckEditorElement | undefined;

  init({mainRef, actionsEditorRef}: {mainRef: HTMLElement; actionsEditorRef: HTMLAppActionsDeckEditorElement | undefined}): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.mainRef = mainRef;
      this.actionsEditorRef = actionsEditorRef;

      const deck: HTMLElement = this.mainRef.querySelector('deckgo-deck');

      if (deck) {
        deck.addEventListener('keyup', this.onKeyUp, false);
      }

      attachPasteEvent();

      document.addEventListener('blockSlide', this.onBlockSlide, false);
      document.addEventListener('keydown', this.onKeyDown, false);

      document.addEventListener('drrSelect', this.onDraggableResizableSelect, false);

      resolve();
    });
  }

  destroy() {
    const deck: HTMLElement = this.mainRef.querySelector('deckgo-deck');

    if (deck) {
      deck.removeEventListener('keyup', this.onKeyUp, true);
    }

    detachPasteEvent();

    document.removeEventListener('blockSlide', this.onBlockSlide, true);
    document.removeEventListener('keydown', this.onKeyDown, true);

    document.removeEventListener('drrSelect', this.onDraggableResizableSelect, true);
  }

  private onKeyUp = async ($event: KeyboardEvent) => {
    if ($event && $event.key === 'Tab' && document && document.activeElement && document.activeElement instanceof HTMLElement) {
      await this.tabElement(document.activeElement);
    }
  };

  private onKeyDown = async ($event: KeyboardEvent) => {
    if ($event && $event.key === 'Escape') {
      await this.selectDeck();
      return;
    }

    if ($event?.metaKey && $event.key === 'z' && !$event?.shiftKey) {
      await undo($event);
      return;
    }

    if ($event?.metaKey && $event.key === 'z' && $event?.shiftKey) {
      await redo($event);
      return;
    }
  };

  private async tabElement(element: HTMLElement) {
    await this.actionsEditorRef?.touch(element);
  }

  async selectDeck() {
    await this.actionsEditorRef.selectDeck();
  }

  private onBlockSlide = async ($event: CustomEvent) => {
    await this.blockSlide($event.detail);
  };

  blockSlide(blockState: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLElement = this.mainRef.querySelector('deckgo-deck');

      if (!deck) {
        resolve();
        return;
      }

      await (deck as any).blockSlide(blockState);
      await (deck as any).toggleKeyboardAssist(!blockState);

      resolve();
    });
  }

  private onDraggableResizableSelect = async ($event: CustomEvent) => {
    await this.blockSlide($event && $event.detail !== undefined);
  };
}
