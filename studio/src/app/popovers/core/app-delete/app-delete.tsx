import {Component, Element, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

@Component({
  tag: 'app-delete',
  styleUrl: 'app-delete.scss'
})
export class AppDelete {
  @Element() el: HTMLElement;

  async closePopover(confirm: boolean) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss(confirm);
  }

  render() {
    return [
      <ion-grid class="ion-no-padding ion-margin">
        <ion-row>
          <h3>{i18n.state.editor.delete_question}</h3>
          <p>
            <small>{i18n.state.editor.action_cannot_undone}</small>
          </p>
        </ion-row>
      </ion-grid>,
      this.renderActions()
    ];
  }

  private renderActions() {
    return (
      <div class="element-delete-actions">
        <button class="no ion-activatable" onClick={() => this.closePopover(false)}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-label>{i18n.state.core.no}</ion-label>
        </button>

        <button class="yes ion-activatable" onClick={() => this.closePopover(true)}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-label>
            <strong>{i18n.state.core.yes}</strong>
          </ion-label>
        </button>
      </div>
    );
  }
}
