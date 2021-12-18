import {Component, Element, h} from '@stencil/core';
import i18n from '../../../../stores/i18n.store';

@Component({
  tag: 'app-cloud-wait',
  styleUrl: 'app-cloud-wait.scss'
})
export class AppCloudWait {
  @Element() el: HTMLElement;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  render() {
    return (
      <div class="ion-padding">
        <p>{i18n.state.sync.wait}</p>
        <div class="ion-text-center">
          <ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>
            {i18n.state.core.got_it}
          </ion-button>
        </div>
      </div>
    );
  }
}
