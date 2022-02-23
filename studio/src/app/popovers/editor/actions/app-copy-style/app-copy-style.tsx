import {Component, Element, h, Prop} from '@stencil/core';
import deckEditorStore from '../../../../stores/deck-editor.store';
import i18n from '../../../../stores/i18n.store';

@Component({
  tag: 'app-copy-style',
  styleUrl: 'app-copy-style.scss'
})
export class AppCopyStyle {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async copyStyle() {
    const style: string | null = this.selectedTarget?.getAttribute('style');
    deckEditorStore.state.style = style && style !== '' ? style : null;

    await this.closePopover();
  }

  private async applyStyle() {
    if (!deckEditorStore.state.style) {
      return;
    }

    this.selectedTarget.setAttribute('style', deckEditorStore.state.style);

    await this.closePopover();
  }

  render() {
    return (
      <div class="ion-padding">
        <a onClick={() => this.copyStyle()} aria-label={i18n.state.editor.copy_style}>
          <p>{i18n.state.editor.copy_style}</p>
        </a>

        <a
          onClick={() => this.applyStyle()}
          aria-label={i18n.state.editor.apply_style}
          class={deckEditorStore.state.style !== null ? undefined : 'disabled'}
        >
          <p>{i18n.state.editor.apply_style}</p>
        </a>
      </div>
    );
  }
}
