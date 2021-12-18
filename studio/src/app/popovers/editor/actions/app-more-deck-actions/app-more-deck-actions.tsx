import {Component, Element, h} from '@stencil/core';

import i18n from '../../../../stores/i18n.store';

import {MoreAction} from '../../../../types/editor/more-action';

@Component({
  tag: 'app-more-deck-actions',
  styleUrl: 'app-more-deck-actions.scss'
})
export class AppMoreDeckActions {
  @Element() el: HTMLElement;

  private async closePopover(action: MoreAction) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      action: action
    });
  }

  render() {
    return (
      <div class="ion-padding-start-ion-padding-end">
        <a onClick={() => this.closePopover(MoreAction.JUMP_TO)} aria-label={i18n.state.editor.slides}>
          <p>{i18n.state.editor.slides}</p>
        </a>

        <a onClick={() => this.closePopover(MoreAction.PRESENT)}>
          <p>{i18n.state.editor.present}</p>
        </a>

        <app-action-help link={true} onHelpSelected={() => this.closePopover(MoreAction.HELP)}></app-action-help>
      </div>
    );
  }
}
