import {throwError} from '@deckdeckgo/editor';
import {clone, DeckOrDoc} from '@deckdeckgo/sync';
import type {OverlayEventDetail} from '@ionic/core';
import {loadingController, popoverController} from '@ionic/core';
import {Component, Event, EventEmitter, h, Host, Prop, State} from '@stencil/core';
import i18n from '../../../stores/i18n.store';
import {deleteDeckOrDoc} from '../../../utils/core/dashboard.utils';
import {AppIcon} from '../../core/app-icon/app-icon';

@Component({
  tag: 'app-dashboard-actions',
  styleUrl: 'app-dashboard-actions.scss'
})
export class AppDashboardActions {
  @Prop()
  data: DeckOrDoc;

  @Prop()
  disableDelete: boolean;

  @Event() deleted: EventEmitter<string>;
  @Event() cloned: EventEmitter<void>;

  @State()
  private actionInProgress: boolean = false;

  private async presentConfirmDelete($event: UIEvent) {
    $event.stopPropagation();

    if (this.actionInProgress) {
      return;
    }

    if (!this.data) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-delete',
      event: $event,
      mode: 'ios'
    });

    popover.onDidDismiss().then(async ({data}: OverlayEventDetail) => {
      if (data) {
        await this.deleteData();
      }
    });

    await popover.present();
  }

  private deleteData(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.data) {
        resolve();
        return;
      }

      this.actionInProgress = true;

      const loading: HTMLIonLoadingElement = await loadingController.create({});

      await loading.present();

      try {
        await deleteDeckOrDoc(this.data);

        this.deleted.emit(this.data.deck?.id || this.data.doc?.id);
      } catch (err) {
        throwError(err);
      }

      await loading.dismiss();

      this.actionInProgress = false;

      resolve();
    });
  }

  private async cloneData($event: UIEvent): Promise<void> {
    $event.stopPropagation();

    if (this.actionInProgress) {
      return;
    }

    this.actionInProgress = true;

    const loading: HTMLIonLoadingElement = await loadingController.create({});

    await loading.present();

    try {
      await clone(this.data);

      this.cloned.emit();
    } catch (err) {
      throwError(err);
    }

    await loading.dismiss();

    this.actionInProgress = false;
  }

  render() {
    return (
      <Host>
        <button onClick={($event: UIEvent) => this.cloneData($event)} title={i18n.state.dashboard.copy} disabled={this.actionInProgress}>
          <AppIcon name="copy" ariaLabel="" ariaHidden={true}></AppIcon>
        </button>

        <button
          onClick={($event: UIEvent) => this.presentConfirmDelete($event)}
          title={i18n.state.dashboard.delete}
          disabled={this.actionInProgress || this.disableDelete}>
          <AppIcon name="trash" ariaLabel="" ariaHidden={true}></AppIcon>
        </button>
      </Host>
    );
  }
}
