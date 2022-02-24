import {Component, Element, h} from '@stencil/core';
import i18n from '../../../../stores/i18n.store';
import {MoreAction} from '../../../../types/editor/more-action';

@Component({
  tag: 'app-more-file-actions',
  styleUrl: 'app-more-file-actions.scss'
})
export class AppMoreFileActions {
  @Element() el: HTMLElement;

  private async closePopover(action: MoreAction) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      action: action
    });
  }

  render() {
    return (
      <div class="ion-padding-start ion-padding-end">
        <a onClick={() => this.closePopover(MoreAction.NEW)} aria-label={i18n.state.tools.new}>
          <p>{i18n.state.tools.new}</p>
        </a>

        <a onClick={() => this.closePopover(MoreAction.OPEN)} aria-label={i18n.state.tools.open}>
          <p>{i18n.state.tools.open}</p>
        </a>

        <a onClick={() => this.closePopover(MoreAction.EXPORT)} aria-label={i18n.state.editor.export}>
          <p>{i18n.state.editor.export}</p>
        </a>
      </div>
    );
  }
}
