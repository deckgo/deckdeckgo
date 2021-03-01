import {Component, h, State, Event, EventEmitter} from '@stencil/core';

import errorStore from '../../../../stores/error.store';
import offlineStore from '../../../../stores/offline.store';
import i18n from '../../../../stores/i18n.store';

import {OfflineService} from '../../../../services/editor/offline/offline.service';

import {renderI18n} from '../../../../utils/core/i18n.utils';

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
      errorStore.state.error = i18n.state.offline.error_offline;
    }
  }

  render() {
    return (
      <article>
        <h1>{i18n.state.editor.go_offline}</h1>
        <p>{i18n.state.offline.why}</p>
        <p>
          {renderI18n(i18n.state.offline.turn, {
            placeholder: '{0}',
            value: <strong>{i18n.state.offline.offline}</strong>,
          })}
        </p>
        <p>
          {renderI18n(i18n.state.offline.still_edit, {
            placeholder: '{0}',
            value: <strong>{i18n.state.offline.edit}</strong>,
          })}
        </p>
        <div class="ion-padding ion-text-center go">{this.renderGoOffline()}</div>
      </article>
    );
  }

  private renderGoOffline() {
    if (!this.goingOffline) {
      return (
        <ion-button type="submit" color="tertiary" shape="round" onClick={() => this.goOffline()}>
          <ion-label>{i18n.state.offline.offline_now}</ion-label>
        </ion-button>
      );
    } else {
      return (
        <div class="in-progress">
          <ion-progress-bar value={offlineStore.state.progress} color="tertiary"></ion-progress-bar>
          <ion-label>{i18n.state.offline.hang_on_gather}</ion-label>
        </div>
      );
    }
  }
}
