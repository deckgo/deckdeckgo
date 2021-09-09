import {Component, h, Fragment, Prop} from '@stencil/core';

import type {OverlayEventDetail} from '@ionic/core';

import i18n from '../../../stores/i18n.store';
import navStore, {NavDirection} from '../../../stores/nav.store';

import {AppIcon} from '../app-icon/app-icon';

import {popoverController} from '../../../utils/ionic/ionic.overlay';

@Component({
  tag: 'app-start-deck'
})
export class AppStartDeck {
  @Prop()
  writeColor: 'primary' | 'dark' | 'light' = 'primary';

  @Prop()
  importColor: 'dark' | 'light' = 'dark';

  private async navigateEditor() {
    navStore.state.nav = {
      url: '/editor',
      direction: NavDirection.RELOAD
    };
  }

  private async presentDeckImport($event: UIEvent) {
    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-deck-import',
      event: $event,
      mode: 'ios',
      cssClass: 'info'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data.deckId !== undefined) {
        await this.navigateDeck(detail.data.deckId);
      }
    });

    await popover.present();
  }

  private async navigateDeck(deckId: string) {
    const url: string = `/editor/${deckId}`;

    navStore.state.nav = {
      url: url,
      direction: NavDirection.RELOAD
    };
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
          <AppIcon name="chevron-down-circle" ariaLabel="" ariaHidden={true}></AppIcon>
        </ion-button>
      </Fragment>
    );
  }
}
