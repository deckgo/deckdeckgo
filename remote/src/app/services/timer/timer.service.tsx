import {Observable, interval, Subject, BehaviorSubject, Subscription} from 'rxjs';
import {filter, takeUntil, withLatestFrom} from 'rxjs/operators';

import {differenceInMilliseconds, addMilliseconds, isAfter} from 'date-fns';

import {get, set, del} from 'idb-keyval';

import {Comparator, Converter} from '../utils/utils';
import {NotificationService} from '../notification/notification.service';

export interface TimerInterval {
    timerProgression: number;
    timerRemaining: number;
    timerLength: number;
}

export class TimerService {

    private static instance: TimerService;

    private timerSubject: BehaviorSubject<BehaviorSubject<TimerInterval>> = new BehaviorSubject(null);

    private intervalSubject: BehaviorSubject<TimerInterval>;

    private stopTimer: Subject<void> = new Subject<void>();
    private pauseTimer: Subject<boolean> = new Subject<boolean>();

    private timerPausedAt: Date;

    private stopwatchSubscription: Subscription;

    private timerEnd: Date;
    private timerLength: number;
    private timerProgression: number;

    private notificationService: NotificationService = NotificationService.getInstance();

    private constructor() {
        // Private constructor, singleton
    }

    static getInstance() {
        if (!TimerService.instance) {
            TimerService.instance = new TimerService();
        }
        return TimerService.instance;
    }

    destroy() {
        this.stopTimer.next();

        if (this.stopwatchSubscription) {
            this.stopwatchSubscription.unsubscribe();
        }
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

                this.initTimer();

                this.pauseTimer.next(false);

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

                    this.initTimer();

                    this.pauseTimer.next(false);
                }

                resolve();
            } catch (e) {
                reject(e);
            }
        });
    }

    private initTimer() {
        this.intervalSubject = new BehaviorSubject<TimerInterval>(null);

        this.stopwatchSubscription = interval(1000).pipe(
            takeUntil(this.stopTimer),
            withLatestFrom(this.pauseTimer.asObservable()),
            filter(([_v, paused]) => !paused)).subscribe(async ([_intervalValue, _paused]: [number, boolean]) => {
            if (this.timerEnd) {
                const now: Date = new Date();

                if (isAfter(this.timerEnd, now)) {
                    this.timerProgression = this.timerProgression + 1000;
                    const timerRemaining: number = this.timerLength - this.timerProgression;

                    await set('deckdeckgo_timer_progression', this.timerProgression);

                    this.intervalSubject.next({
                        timerProgression: this.timerProgression,
                        timerRemaining: timerRemaining,
                        timerLength: this.timerLength
                    });

                    await this.showWarnNotification(timerRemaining);
                } else {
                    await this.stop(true);
                }
            }
        });

        this.timerSubject.next(this.intervalSubject);
    }

    watch(): Observable<BehaviorSubject<TimerInterval>> {
        return this.timerSubject.asObservable();
    }

    pause(state: boolean): Promise<void> {
        return new Promise<void>((resolve) => {
            if (state) {
                this.timerPausedAt = new Date();
            } else {
                const pausedFor: number = differenceInMilliseconds(new Date(), this.timerPausedAt);
                this.timerEnd = addMilliseconds(this.timerEnd, pausedFor);
            }

            this.pauseTimer.next(state);

            resolve();
        });
    }

    async stop(notification: boolean) {
        if (!this.intervalSubject) {
            return;
        }

        this.intervalSubject.complete();
        this.stopTimer.next();

        if (notification) {
            // Star Wars shamelessly taken from the awesome Peter Beverloo as in the Google example
            // https://tests.peter.sh/notification-generator/
            await this.notificationService.showNotification('Your presentation is over', [500, 110, 500, 110, 450, 110, 200, 110, 170, 40, 450, 110, 200, 110, 170, 40, 500]);
        }
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
                const end: string = await get('deckdeckgo_timer_end');

                resolve(Comparator.isStringNotEmpty(end));
            } catch (e) {
                resolve(false);
            }
        });
    }

    clearEndAt(): Promise<void> {
        return del('deckdeckgo_timer_end');
    }
}
