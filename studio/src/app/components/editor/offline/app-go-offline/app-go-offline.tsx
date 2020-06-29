import {Component, h, State, Event, EventEmitter} from '@stencil/core';

import errorStore from '../../../../stores/error.store';
import offlineStore from '../../../../stores/offline.store';

import {OfflineService} from '../../../../services/editor/offline/offline.service';

@Component({
  tag: 'app-go-offline',
})
export class AppGoOffline {
  @State()
  private goingOffline: boolean = false;

  @Event()
  private doneOffline: EventEmitter<void>;

  @Event()
  private inProgress: EventEmitter<boolean>;

  private offlineService: OfflineService;

  constructor() {
    this.offlineService = OfflineService.getInstance();
  }

  private async goOffline() {
    this.goingOffline = true;
    this.inProgress.emit(true);

    try {
      await this.offlineService.save();

      this.doneOffline.emit();
    } catch (err) {
      this.goingOffline = false;
      this.inProgress.emit(false);
      errorStore.state.error = 'Apologies, something went wrong and app was unable to go offline.';
    }
  }

  render() {
    return (
      <article>
        <h1>Go offline</h1>
        <p>Low bandwidth? Have to jump in a plane? Or want to present without internet connections?</p>
        <p>
          Turn DeckDeckGo <strong>offline</strong> to store your presentation locally.
        </p>
        <p>
          You are still going to be able to <strong>edit</strong> your slides further.
        </p>
        <div class="ion-padding ion-text-center go">{this.renderGoOffline()}</div>
      </article>
    );
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
          <ion-progress-bar value={offlineStore.state.progress} color="tertiary"></ion-progress-bar>
          <ion-label>Hang on, we are gathering the content.</ion-label>
        </div>
      );
    }
  }
}
