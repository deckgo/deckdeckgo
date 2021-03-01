import {Component, h, State, Event, EventEmitter} from '@stencil/core';

import errorStore from '../../../../stores/error.store';
import offlineStore from '../../../../stores/offline.store';
import i18n from '../../../../stores/i18n.store';

import {OfflineService} from '../../../../services/editor/offline/offline.service';
import {renderI18n} from '../../../../utils/core/i18n.utils';

@Component({
  tag: 'app-go-online',
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

  constructor() {
    this.offlineService = OfflineService.getInstance();
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
      errorStore.state.error = i18n.state.offline.error_online;
    }
  }

  render() {
    return (
      <article>
        <h1>{i18n.state.editor.go_online}</h1>
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

    return [<p>{i18n.state.offline.oopsie}</p>, <p>{i18n.state.offline.check}</p>];
  }

  private renderTextOnline() {
    if (!this.navigatorOnline) {
      return undefined;
    }

    return [
      <p>
        {renderI18n(i18n.state.offline.cool, {
          placeholder: '{0}',
          value: <strong>{i18n.state.offline.online}</strong>,
        })}
      </p>,
      <p>
        {renderI18n(i18n.state.offline.note, {
          placeholder: '{0}',
          value: <strong>{i18n.state.offline.replace}</strong>,
        })}
      </p>,
      <p>{i18n.state.offline.long_story}</p>,
    ];
  }

  private renderGoOnline() {
    if (!this.goingOnline) {
      return (
        <ion-button type="submit" color="tertiary" shape="round" onClick={() => this.goOnline()} disabled={!this.navigatorOnline}>
          <ion-label>{i18n.state.offline.online_now}</ion-label>
        </ion-button>
      );
    } else {
      return (
        <div class="in-progress">
          <ion-progress-bar value={offlineStore.state.progress} color="tertiary"></ion-progress-bar>
          <ion-label>{i18n.state.offline.hang_on_upload}</ion-label>
        </div>
      );
    }
  }
}
