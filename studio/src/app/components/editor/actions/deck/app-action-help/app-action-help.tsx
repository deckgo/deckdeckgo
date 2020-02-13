import {Component, h, Element} from '@stencil/core';

import {popoverController} from '@ionic/core';

@Component({
  tag: 'app-action-help'
})
export class AppActionHelp {
  @Element() el: HTMLElement;

  private async openGetHelp() {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-get-help',
      mode: 'ios',
      cssClass: 'info'
    });

    await popover.present();
  }

  render() {
    return (
      <ion-tab-button onClick={() => this.openGetHelp()} color="primary" mode="md" class="get-help-action">
        <ion-icon src="/assets/icons/ionicons/md-help.svg"></ion-icon>
        <ion-label>Help</ion-label>
      </ion-tab-button>
    );
  }
}
