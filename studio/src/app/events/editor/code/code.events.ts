import {modalController, OverlayEventDetail} from '@ionic/core';

export class CodeEvents {
  init() {
    document.addEventListener('editCode', this.onEditCode);
  }

  destroy() {
    document.removeEventListener('editCode', this.onEditCode);
  }

  private onEditCode = async ({target}: CustomEvent<void>) => {
    this.emitSnapshotParagraph(target as HTMLElement);

    const code: HTMLElement | null = (target as HTMLElement).querySelector(':scope > code');

    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-code-editor',
      componentProps: {
        code: code?.innerHTML || ''
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

        (target as HTMLElement).append(slot);
      } else {
        code.innerHTML = innerHTML;
      }

      await (target as HTMLDeckgoHighlightCodeElement).load();

      this.emitCodeDidChange(target as HTMLElement);
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
