import {Component, Element, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';
import syncStore from '../../../stores/sync.store';

@Component({
  tag: 'app-sync-info',
  styleUrl: 'app-sync-info.scss'
})
export class AppSyncInfo {
  @Element() el: HTMLElement;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  render() {
    const label: string =
      syncStore.state.sync === 'error'
        ? i18n.state.sync.cloud_error
        : syncStore.state.sync === 'in_progress'
        ? i18n.state.sync.cloud_in_progress
        : syncStore.state.sync === 'init'
        ? i18n.state.sync.cloud_init
        : syncStore.state.sync === 'pending'
        ? i18n.state.sync.cloud_pending
        : i18n.state.sync.cloud_idle;

    return (
      <div class="ion-padding">
        <h2>{i18n.state.sync.status}</h2>
        <p>{i18n.state.sync.info}</p>
        <p>{label}</p>
        <div class="ion-text-center">
          <ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>
            {i18n.state.core.got_it}
          </ion-button>
        </div>
      </div>
    );
  }
}
