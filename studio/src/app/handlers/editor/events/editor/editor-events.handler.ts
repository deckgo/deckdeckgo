import { redo, undo } from "../../../../helpers/editor/editor.helper";

export class EditorEventsHandler {
  private mainRef: HTMLElement;

  init(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.mainRef = el;

      const deck: HTMLElement = this.mainRef.querySelector('deckgo-deck');

      if (deck) {
        deck.addEventListener('keyup', this.onKeyUp, false);
      }

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

    document.removeEventListener('blockSlide', this.onBlockSlide, true);
    document.removeEventListener('keydown', this.onKeyDown, true);

    document.removeEventListener('drrSelect', this.onDraggableResizableSelect, true);
  }

  private onKeyUp = async ($event: KeyboardEvent) => {
    if ($event && $event.key === 'Tab' && document && document.activeElement && document.activeElement instanceof HTMLElement) {
      await this.touchToolbar(document.activeElement);
    }
  };

  private onKeyDown = async ($event: KeyboardEvent) => {
    if ($event && $event.key === 'Escape') {
      await this.selectDeck();
      return;
    }

    if ($event?.metaKey && $event.key === 'z' && !$event?.shiftKey) {
      await undo();
      return;
    }

    if ($event?.metaKey && $event.key === 'z' && $event?.shiftKey) {
      await redo();
      return;
    }
  };

  private touchToolbar(element: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const actions: HTMLAppActionsEditorElement = this.mainRef.querySelector('app-actions-editor');

      if (!actions) {
        resolve();
        return;
      }

      await actions.touch(element);

      resolve();
    });
  }

  selectDeck(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const actions: HTMLAppActionsEditorElement = this.mainRef.querySelector('app-actions-editor');

      if (actions) {
        await actions.selectDeck();
      }

      resolve();
    });
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
