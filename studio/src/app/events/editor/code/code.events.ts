import type {MonacoEditorOptions} from '@deckdeckgo/monaco-editor';
import {modalController, OverlayEventDetail} from '@ionic/core';

export class CodeEvents {
  init() {
    document.addEventListener('editCode', this.onEditCode);
  }

  destroy() {
    document.removeEventListener('editCode', this.onEditCode);
  }

  private onEditCode = async ({target}: CustomEvent<void>) => {
    await this.editCode(target as HTMLDeckgoHighlightCodeElement);
  };

  private editCode = async (highlightCodeElement: HTMLDeckgoHighlightCodeElement) => {
    this.emitSnapshotParagraph(highlightCodeElement);

    const code: HTMLElement | null = highlightCodeElement.querySelector(':scope > code');

    const language: string | null = highlightCodeElement.getAttribute('language');

    const options: MonacoEditorOptions = {
      ...(language !== null && language !== '' && {language})
    };

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-code-editor',
      componentProps: {
        code: code?.innerHTML || '',
        options
      }
    });

    modal.onDidDismiss().then(async ({data}: OverlayEventDetail) => {
      if (!data) {
        // User aborted edition
        return;
      }

      const {code: innerHTML} = data || {code: '\u200B'};

      if (!code) {
        // Should not happen, decgo-highlight-code are always create with a predefined even empty code slot
        const slot: HTMLElement = document.createElement('code');
        slot.setAttribute('slot', 'code');
        slot.innerHTML = innerHTML;

        highlightCodeElement.append(slot);
      } else {
        code.innerHTML = innerHTML;
      }

      await highlightCodeElement.load();

      this.emitCodeDidChange(highlightCodeElement);
    });

    await modal.present();
  };

  private emitCodeDidChange(target: HTMLElement) {
    const didUpdate: CustomEvent<HTMLElement> = new CustomEvent<HTMLElement>('codeDidChange', {
      bubbles: true,
      detail: target
    });

    target.dispatchEvent(didUpdate);
  }

  // Copy current paragraph for undo-redo "update" in case of changes
  private emitSnapshotParagraph(target: HTMLElement) {
    const didUpdate: CustomEvent<HTMLElement> = new CustomEvent<HTMLElement>('snapshotParagraph', {
      bubbles: true,
      detail: target
    });

    target.dispatchEvent(didUpdate);
  }
}
