import {Component, Element, State, h, Listen} from '@stencil/core';
import {DatetimeChangeEventDetail} from '@ionic/core';

import {differenceInMilliseconds, isAfter, startOfDay} from 'date-fns';

import timerStore from '../../stores/timer.store';

import {Comparator} from '../../services/utils/utils';

import {NotificationService} from '../../services/notification/notification.service';
import {TimerService} from '../../services/timer/timer.service';

@Component({
  tag: 'app-timer',
  styleUrl: 'app-timer.scss',
})
export class AppTimer {
  @Element() el: HTMLElement;

  @State()
  private timerRunning: boolean;

  @State()
  private timerPause: boolean = false;

  private timerService: TimerService;
  private notificationService: NotificationService;

  private destroyListener;

  constructor() {
    this.timerService = TimerService.getInstance();
    this.notificationService = NotificationService.getInstance();
  }

  async componentDidLoad() {
    this.notificationService.init();

    this.timerRunning = await this.timerService.isTimerStarted();

    if (this.timerRunning) {
      await this.toggleFabActivated();
    }
  }

  async disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  async startTimer(length: number) {
    await this.timerService.start(length);
    this.timerRunning = true;
  }

  async stopTimer() {
    await this.timerService.stop(false);
  }

  async pauseTimer(e: UIEvent, pause: boolean) {
    e.stopPropagation();

    await this.timerService.pause(pause);
    this.timerPause = pause;
  }

  @Listen('stopTimer', {target: 'document'})
  async clearStopwatch() {
    this.timerRunning = false;

    await this.resetDatetime();
  }

  private async resetDatetime() {
    await this.deactivatedFab();

    const datetimeElement: HTMLIonDatetimeElement = this.el.querySelector('ion-datetime');

    if (datetimeElement) {
      datetimeElement.value = startOfDay(new Date()).toDateString();
    }
  }

  private openDatetime(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const datetimeElement: HTMLIonDatetimeElement = this.el.querySelector('ion-datetime');

      if (!datetimeElement) {
        resolve();
        return;
      }

      await datetimeElement.open();

      resolve();
    });
  }

  private initTimerLengthAndStartTimer($event: CustomEvent<DatetimeChangeEventDetail>): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail || Comparator.isStringEmpty($event.detail.value)) {
        resolve();
        return;
      }

      if (this.timerRunning) {
        // ion-datetime fire twice on select
        return;
      }

      try {
        const selected: Date = new Date($event.detail.value);
        const todayStart: Date = startOfDay(new Date());

        if (isAfter(selected, todayStart)) {
          await this.startTimer(differenceInMilliseconds(selected, todayStart));
        }
      } catch (err) {
        // Do nothing
      }

      resolve();
    });
  }

  private startStopAction(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.notificationService.askPermission();

      const ionFabElement: HTMLIonFabElement = this.el.querySelector('ion-fab');

      if (!ionFabElement) {
        resolve();
        return;
      }

      // Safari needs a bit to populate attribute activated
      setTimeout(async () => {
        if (ionFabElement.activated) {
          await this.openDatetime();
        } else {
          await this.stopTimer();
        }
      }, 100);

      resolve();
    });
  }

  private startAction(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.notificationService.askPermission();

      await this.toggleFabActivated();

      await this.openDatetime();

      resolve();
    });
  }

  private toggleFabActivated(): Promise<void> {
    return new Promise<void>((resolve) => {
      const ionFabElement: HTMLIonFabElement = this.el.querySelector('ion-fab');

      if (!ionFabElement) {
        resolve();
        return;
      }

      ionFabElement.activated = !ionFabElement.activated;

      resolve();
    });
  }

  private deactivatedFab(): Promise<void> {
    return new Promise<void>((resolve) => {
      const ionFabElement: HTMLIonFabElement = this.el.querySelector('ion-fab');

      if (!ionFabElement || !ionFabElement.activated) {
        resolve();
        return;
      }

      ionFabElement.activated = !ionFabElement.activated;

      resolve();
    });
  }

  render() {
    return [
      <app-header></app-header>,

      <ion-content>
        {this.renderContent()}
        {this.renderActions()}
        <ion-datetime
          display-format="HH:mm"
          pickerOptions={{backdropDismiss: false}}
          value={startOfDay(new Date()).toDateString()}
          onIonCancel={() => this.toggleFabActivated()}
          onIonChange={(e: CustomEvent<DatetimeChangeEventDetail>) => this.initTimerLengthAndStartTimer(e)}></ion-datetime>
      </ion-content>,
    ];
  }

  private renderContent() {
    if (timerStore.state.remainingTime >= 0 && timerStore.state.timer) {
      return (
        <div class="content">
          <app-stopwatch length={timerStore.state.timer.timerLength} remaining={timerStore.state.remainingTime}></app-stopwatch>
        </div>
      );
    } else {
      return (
        <main>
          <h1 class="ion-padding-start ion-padding-end ion-padding-top">The DeckDeckGo remote timer</h1>
          <a onClick={() => this.openDatetime()} class="link-to-timer">
            <p class="ion-padding-start ion-padding-end">Not timer running.</p>
          </a>
          <div class="deck-action-button deck-action-button-screen-center">
            <button onClick={() => this.startAction()} aria-label="Start timer" style={{'--action-button-background': 'var(--ion-color-primary'}}>
              <div>
                <ion-icon name="stopwatch" class="deck-action-button-icon-stopwatch"></ion-icon>
              </div>

              <ion-label>Start a timer</ion-label>
            </button>
          </div>
        </main>
      );
    }
  }

  private renderActions() {
    if (this.timerRunning === null || (this.timerRunning && timerStore.state.remainingTime === null)) {
      return undefined;
    }

    const style = {visibility: `${timerStore.state.remainingTime === null || timerStore.state.remainingTime === undefined ? 'hidden' : 'inherit'}`};

    return (
      <ion-fab vertical="bottom" horizontal="end" slot="fixed" onClick={(e: UIEvent) => e.stopPropagation()} style={style}>
        <ion-fab-button onClick={() => this.startStopAction()}>
          <ion-icon name="stopwatch"></ion-icon>
        </ion-fab-button>
        <ion-fab-list side="start">{this.renderActionsPause()}</ion-fab-list>
      </ion-fab>
    );
  }

  private renderActionsPause() {
    if (this.timerPause) {
      return (
        <ion-fab-button color="medium" onClick={(e: UIEvent) => this.pauseTimer(e, false)}>
          <ion-icon name="play"></ion-icon>
        </ion-fab-button>
      );
    } else {
      return (
        <ion-fab-button color="medium" onClick={(e: UIEvent) => this.pauseTimer(e, true)}>
          <ion-icon name="pause"></ion-icon>
        </ion-fab-button>
      );
    }
  }
}
