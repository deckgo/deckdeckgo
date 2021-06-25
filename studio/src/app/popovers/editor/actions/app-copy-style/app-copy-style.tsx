import {Component, Element, h, Prop} from '@stencil/core';

import editorStore from '../../../../stores/editor.store';
import i18n from '../../../../stores/i18n.store';

@Component({
  tag: 'app-copy-style',
  styleUrl: 'app-copy-style.scss'
})
export class AppCopyStyle {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async copyStyle() {
    const style: string | null = this.selectedElement?.getAttribute('style');
    editorStore.state.style = style && style !== '' ? style : null;

    await this.closePopover();
  }

  private async applyStyle() {
    if (!editorStore.state.style) {
      return;
    }

    this.selectedElement.setAttribute('style', editorStore.state.style);

    await this.closePopover();
  }

  render() {
    return (
      <div class="ion-padding">
        <a onClick={() => this.copyStyle()} aria-label={i18n.state.editor.copy_style}>
          <p>{i18n.state.editor.copy_style}</p>
        </a>

        <a onClick={() => this.applyStyle()} aria-label={i18n.state.editor.apply_style} class={editorStore.state.style !== null ? undefined : 'disabled'}>
          <p>{i18n.state.editor.apply_style}</p>
        </a>
      </div>
    );
  }
}
