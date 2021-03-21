import {Component, h, Fragment, Prop} from '@stencil/core';

import {OverlayEventDetail, popoverController} from '@ionic/core';

import i18n from '../../../stores/i18n.store';
import navStore, {NavDirection} from '../../../stores/nav.store';

@Component({
  tag: 'app-start-deck',
})
export class AppStartDeck {
  @Prop()
  writeColor: 'primary' | 'dark' | 'light' = 'primary';

  @Prop()
  importColor: 'medium' | 'dark' | 'light' = 'medium';

  private async navigateEditor() {
    navStore.state.nav = {
      url: '/editor',
      direction: NavDirection.RELOAD,
    };
  }

  private async presentDeckImport($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-deck-import',
      event: $event,
      mode: 'ios',
    });

    popover.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
      // TODO: Refresh
    });

    await popover.present();
  }

  render() {
    return (
      <Fragment>
        <ion-button shape="round" color={this.writeColor} onClick={() => this.navigateEditor()}>
          <ion-label>{i18n.state.nav.write_a_presentation}</ion-label>
        </ion-button>

        <ion-button
          fill="clear"
          color={this.importColor}
          onClick={($event: UIEvent) => this.presentDeckImport($event)}
          style={{margin: '0 16px 0 4px'}}
          class="ion-no-padding">
          <ion-icon name="chevron-down-circle-outline" aria-label={i18n.state.dashboard.import}></ion-icon>
        </ion-button>
      </Fragment>
    );
  }
}
