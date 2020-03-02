import {Component, h, State, Event, EventEmitter} from '@stencil/core';

import {OfflineService} from '../../../../services/editor/offline/offline.service';

@Component({
  tag: 'app-go-offline'
})
export class AppGoOffline {
  @State()
  private goingOffline: boolean = false;

  @State()
  private progress: number = 0;

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
      <p>Your bandwidth is too low or inconsistent? You have to take the plane? Or have to present or work without internet connection?</p>,
      <p>Turn DeckDeckGo offline to store your presentation locally while being still able to edit your slides.</p>,
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
          <ion-progress-bar value={this.progress} color="tertiary"></ion-progress-bar>
          <ion-label>Hang on, we are gathering the content.</ion-label>
        </div>
      );
    }
  }
}
