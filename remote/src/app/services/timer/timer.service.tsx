import {Build} from '@stencil/core';

import {differenceInMilliseconds, addMilliseconds, isAfter} from 'date-fns';

import {get, set, del} from 'idb-keyval';

import timerStore from '../../stores/timer.store';

import {Comparator, Converter} from '../utils/utils';

import {NotificationService} from '../notification/notification.service';

export class TimerService {
  private static instance: TimerService;

  private timerPausedAt: Date;

  private timerEnd: Date;
  private timerLength: number;
  private timerProgression: number;

  private notificationService: NotificationService = NotificationService.getInstance();

  private timerInterval: NodeJS.Timeout;

  private pauseTimer: boolean = false;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!TimerService.instance) {
      TimerService.instance = new TimerService();
    }
    return TimerService.instance;
  }

  async destroy() {
    await this.clear();
  }

  private async clear() {
    timerStore.reset();

    if (this.timerInterval) {
      clearInterval(this.timerInterval);
    }

    await del('deckdeckgo_timer_end');
    await del('deckdeckgo_timer_length');
    await del('deckdeckgo_timer_progression');
  }

  start(length: number): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        this.timerEnd = addMilliseconds(new Date(), length);
        this.timerLength = length;
        this.timerProgression = 0;

        await set('deckdeckgo_timer_end', this.timerEnd.toISOString());
        await set('deckdeckgo_timer_length', this.timerLength);
        await set('deckdeckgo_timer_progression', this.timerProgression);

        this.pauseTimer = false;

        this.initTimer();

        resolve();
      } catch (e) {
        reject(e);
      }
    });
  }

  restart(): Promise<void> {
    return new Promise<void>(async (resolve, reject) => {
      try {
        const isTimerStarted: boolean = await this.isTimerStarted();

        if (isTimerStarted) {
          this.timerEnd = await this.getTimerEndAt();
          this.timerLength = parseInt(await get('deckdeckgo_timer_length'), 0);
          this.timerProgression = parseInt(await get('deckdeckgo_timer_progression'), 0);

          this.pauseTimer = false;

          this.initTimer();
        }

        resolve();
      } catch (e) {
        reject(e);
      }
    });
  }

  private initTimer() {
    this.timerInterval = setInterval(async () => {
      if (this.timerEnd && !this.pauseTimer) {
        const now: Date = new Date();

        if (isAfter(this.timerEnd, now)) {
          this.timerProgression = this.timerProgression + 1000;
          const timerRemaining: number = this.timerLength - this.timerProgression;

          await set('deckdeckgo_timer_progression', this.timerProgression);

          timerStore.state.timer = {
            timerProgression: this.timerProgression,
            timerRemaining: timerRemaining,
            timerLength: this.timerLength
          };

          await this.showWarnNotification(timerRemaining);
        } else {
          await this.stop(true);
        }
      }
    }, 1000);
  }

  async pause(state: boolean) {
    this.pauseTimer = state;

    if (state) {
      this.timerPausedAt = new Date();
    } else {
      const pausedFor: number = differenceInMilliseconds(new Date(), this.timerPausedAt);
      this.timerEnd = addMilliseconds(this.timerEnd, pausedFor);
    }
  }

  async stop(notification: boolean) {
    await this.clear();

    this.emitStepEvent();

    if (notification) {
      // Star Wars shamelessly taken from the awesome Peter Beverloo as in the Google example
      // https://tests.peter.sh/notification-generator/
      await this.notificationService.showNotification(
        'Your presentation is over',
        [500, 110, 500, 110, 450, 110, 200, 110, 170, 40, 450, 110, 200, 110, 170, 40, 500]
      );
    }
  }

  private emitStepEvent() {
    const stopTimer: CustomEvent<void> = new CustomEvent<void>('stopTimer', {
      bubbles: true
    });

    document.dispatchEvent(stopTimer);
  }

  private showWarnNotification(timerRemaining: number): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (timerRemaining === 60000) {
        await this.notificationService.showNotification('Your presentation is almost over', [500]);
      } else if (timerRemaining === 30000) {
        await this.notificationService.showNotification('Your presentation end in 30 seconds', [500, 110, 500]);
      }

      resolve();
    });
  }

  private getTimerEndAt(): Promise<Date> {
    return new Promise<Date>(async (resolve, reject) => {
      try {
        const end: string = await get('deckdeckgo_timer_end');
        resolve(Converter.getDateObj(end));
      } catch (err) {
        reject(err);
      }
    });
  }

  isTimerStarted(): Promise<boolean> {
    return new Promise<boolean>(async (resolve) => {
      try {
        if (!Build.isBrowser) {
          resolve(false);
          return;
        }

        const end: string = await get('deckdeckgo_timer_end');

        resolve(Comparator.isStringNotEmpty(end));
      } catch (e) {
        resolve(false);
      }
    });
  }
}
