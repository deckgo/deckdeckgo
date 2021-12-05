import {redo, undo} from '../../../utils/editor/undo-redo.deck.utils';
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
        deck.addEventListener('keyup', this.onKeyUp);
      }

      attachPasteEvent();

      document.addEventListener('blockSlide', this.onBlockSlide);
      document.addEventListener('keydown', this.onKeyDown);

      document.addEventListener('drrSelect', this.onDraggableResizableSelect);

      resolve();
    });
  }

  destroy() {
    const deck: HTMLElement = this.mainRef.querySelector('deckgo-deck');

    if (deck) {
      deck.removeEventListener('keyup', this.onKeyUp);
    }

    detachPasteEvent();

    document.removeEventListener('blockSlide', this.onBlockSlide);
    document.removeEventListener('keydown', this.onKeyDown);

    document.removeEventListener('drrSelect', this.onDraggableResizableSelect);
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
