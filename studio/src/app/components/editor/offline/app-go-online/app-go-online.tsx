import {Component, h, State, Event, EventEmitter} from '@stencil/core';

import {OfflineService} from '../../../../services/editor/offline/offline.service';
import {ErrorService} from '../../../../services/core/error/error.service';

@Component({
  tag: 'app-go-online'
})
export class AppGoOnline {
  @State()
  private goingOnline: boolean = false;

  @State()
  private navigatorOnline: boolean = navigator.onLine;

  @Event()
  private doneOnline: EventEmitter<void>;

  @Event()
  private inProgress: EventEmitter<boolean>;

  private offlineService: OfflineService;
  private errorService: ErrorService;

  constructor() {
    this.offlineService = OfflineService.getInstance();
    this.errorService = ErrorService.getInstance();
  }

  private async goOnline() {
    this.goingOnline = true;
    this.inProgress.emit(true);

    try {
      await this.offlineService.upload();

      this.doneOnline.emit();
    } catch (err) {
      this.goingOnline = false;
      this.inProgress.emit(false);
      this.errorService.error('Something went wrong. Double check your internet connection and try again. If it still does not work, contact us!');
    }
  }

  render() {
    return [this.renderTextOffline(), this.renderTextOnline(), <div class="ion-padding ion-text-center go">{this.renderGoOnline()}</div>];
  }

  private renderTextOffline() {
    if (this.navigatorOnline) {
      return undefined;
    }

    return [<p>Oopsie, you are not online yet.</p>, <p>Check your online connection, refresh your browser and try again!</p>];
  }

  private renderTextOnline() {
    if (!this.navigatorOnline) {
      return undefined;
    }

    return [
      <p>Cool, you are online again.</p>,
      <p>
        Please note that the upload of this deck will <strong>replace</strong> its previous online version.
      </p>,
      <p>Long story short, your local presentation is going to be uploaded and saved in the database as the good one.</p>
    ];
  }

  private renderGoOnline() {
    if (!this.goingOnline) {
      return (
        <ion-button type="submit" color="tertiary" shape="round" onClick={() => this.goOnline()} disabled={!this.navigatorOnline}>
          <ion-label>Go online now</ion-label>
        </ion-button>
      );
    } else {
      return (
        <div class="in-progress">
          <ion-spinner color="tertiary"></ion-spinner>
          <ion-label>Hang on still, we are uploading the content.</ion-label>
        </div>
      );
    }
  }
}
