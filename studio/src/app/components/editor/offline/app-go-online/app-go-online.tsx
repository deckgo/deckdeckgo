import {Component, h, State, Event, EventEmitter} from '@stencil/core';

import {Subscription} from 'rxjs';

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

  @State()
  private progress: number = 0;

  private offlineService: OfflineService;
  private errorService: ErrorService;

  private progressSubscription: Subscription;

  constructor() {
    this.offlineService = OfflineService.getInstance();
    this.errorService = ErrorService.getInstance();
  }

  componentWillLoad() {
    this.progressSubscription = this.offlineService.watchProgress().subscribe((progress: number) => {
      this.progress = progress;
    });
  }

  componentDidUnload() {
    if (this.progressSubscription) {
      this.progressSubscription.unsubscribe();
    }
  }

  private async goOnline() {
    this.goingOnline = true;
    this.inProgress.emit(true);

    try {
      await this.offlineService.upload();

      // We are all good, just a small delay to display the progress bar to 100%
      setTimeout(() => {
        this.doneOnline.emit();
      }, 300);
    } catch (err) {
      this.goingOnline = false;
      this.inProgress.emit(false);
      this.errorService.error('Something went wrong. Double check your internet connection and try again. If it still does not work, contact us!');
    }
  }

  render() {
    return (
      <article>
        <h1>Go online</h1>
        {this.renderTextOffline()}
        {this.renderTextOnline()}
        <div class="ion-padding ion-text-center go">{this.renderGoOnline()}</div>
      </article>
    );
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
      <p>
        Cool, you are back <strong>online</strong>.
      </p>,
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
          <ion-progress-bar value={this.progress} color="tertiary"></ion-progress-bar>
          <ion-label>Hang on still, we are uploading the content.</ion-label>
        </div>
      );
    }
  }
}
