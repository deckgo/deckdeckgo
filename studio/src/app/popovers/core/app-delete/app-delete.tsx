import {Component, Element, Fragment, h} from '@stencil/core';
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
    return (
      <Fragment>
        <ion-grid class="ion-no-padding ion-margin">
          <ion-row>
            <h3>{i18n.state.editor.delete_question}</h3>
            <p>
              <small>{i18n.state.editor.action_cannot_undone}</small>
            </p>
          </ion-row>
        </ion-grid>
        <app-popover-confirm onConfirm={({detail}: CustomEvent<boolean>) => this.closePopover(detail)}></app-popover-confirm>
      </Fragment>
    );
  }
}
