export class EditorEventsHandler {
  private el: HTMLElement;

  init(el: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.el = el;

      const deck: HTMLElement = this.el.querySelector('deckgo-deck');

      if (deck) {
        deck.addEventListener('keyup', this.onKeyUp, false);
      }

      document.addEventListener('blockSlide', this.onBlockSlide, false);
      document.addEventListener('keydown', this.onKeyDown, false);

      document.addEventListener('dnrSelect', this.onDraggableResizableSelect, false);

      resolve();
    });
  }

  destroy() {
    const deck: HTMLElement = this.el.querySelector('deckgo-deck');

    if (deck) {
      deck.removeEventListener('keyup', this.onKeyUp, true);
    }

    document.removeEventListener('blockSlide', this.onBlockSlide, true);
    document.removeEventListener('keydown', this.onKeyDown, true);

    document.removeEventListener('dnrSelect', this.onDraggableResizableSelect, true);
  }

  private onKeyUp = async ($event: KeyboardEvent) => {
    if ($event && $event.key === 'Tab' && document && document.activeElement && document.activeElement instanceof HTMLElement) {
      await this.touchToolbar(document.activeElement);
    }
  };

  private onKeyDown = async ($event: KeyboardEvent) => {
    if ($event && $event.key === 'Escape') {
      await this.selectDeck();
    }
  };

  private touchToolbar(element: HTMLElement): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const toolbar: HTMLAppEditorToolbarElement = this.el.querySelector('app-editor-toolbar');

      if (!toolbar) {
        resolve();
        return;
      }

      await toolbar.touch(element);

      resolve();
    });
  }

  selectDeck(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const toolbar: HTMLAppEditorToolbarElement = this.el.querySelector('app-editor-toolbar');

      if (toolbar) {
        await toolbar.blurSelectedElement();
        await toolbar.unSelect();
      }

      await this.blockSlide(false);

      resolve();
    });
  }

  private onBlockSlide = async ($event: CustomEvent) => {
    await this.blockSlide($event.detail);
  };

  blockSlide(blockState: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const deck: HTMLElement = this.el.querySelector('deckgo-deck');

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
