import {Component, h, State, Event, EventEmitter} from '@stencil/core';

import {OfflineService} from '../../../../services/editor/offline/offline.service';

@Component({
  tag: 'app-go-offline'
})
export class AppGoOffline {
  @State()
  private goingOffline: boolean = false;

  @Event()
  private doneOffline: EventEmitter<void>;

  private offlineService: OfflineService;

  constructor() {
    this.offlineService = OfflineService.getInstance();
  }

  private async goOffline() {
    this.goingOffline = true;

    try {
      await this.offlineService.save();

      this.doneOffline.emit();
    } catch (err) {
      this.goingOffline = false;
      // TODO ERROR
    }
  }

  render() {
    return [
      <p>Low bandwidth? Have to take a plane? Or want to present without internet connections?</p>,
      <p>
        Turn DeckDeckGo <strong>offline</strong> to store your presentation locally while being still able to edit your slides further.
      </p>,
      <div class="ion-padding ion-text-center go">{this.renderGoOffline()}</div>
    ];
  }

  private renderGoOffline() {
    if (!this.goingOffline) {
      return (
        <ion-button type="submit" color="tertiary" shape="round" onClick={() => this.goOffline()}>
          <ion-label>Go offline now</ion-label>
        </ion-button>
      );
    } else {
      return (
        <div class="in-progress">
          <ion-spinner color="tertiary"></ion-spinner>
          <ion-label>Hang on, we are gathering the content.</ion-label>
        </div>
      );
    }
  }
}
