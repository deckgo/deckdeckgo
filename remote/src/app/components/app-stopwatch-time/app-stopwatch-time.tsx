import {Component, State, h} from '@stencil/core';

import {BehaviorSubject, Subscription} from 'rxjs';

import {addMilliseconds, formatDistanceStrict} from "date-fns";

import {TimerInterval, TimerService} from '../../services/timer/timer.service';

@Component({
    tag: 'app-stopwatch-time',
    styleUrl: 'app-stopwatch-time.scss'
})
export class AppStopwatchTime {

    private watcherSubscription: Subscription;
    private timerSubscription: Subscription;

    @State()
    private remainingText: string;

    @State()
    private remainingTime: number = 0;

    private timerService: TimerService;

    constructor() {
        this.timerService = TimerService.getInstance();
    }

    async componentWillLoad() {
        this.watcherSubscription = this.timerService.watch().subscribe((timer: BehaviorSubject<TimerInterval>) => {
            if (timer) {
                this.timerSubscription = timer.subscribe((interval: TimerInterval) => {
                        if (interval) {
                            this.remainingTime = interval.timerRemaining;

                            const end: Date = addMilliseconds(new Date(), this.remainingTime);
                            this.remainingText = formatDistanceStrict(end, new Date());
                        } else {
                            this.remainingTime = 0;
                            this.remainingText = formatDistanceStrict(new Date(), new Date());
                        }
                    },
                    (_err) => {
                        // Do nothing
                    }, async () => {
                        this.remainingTime = 0;
                        this.remainingText = formatDistanceStrict(new Date(), new Date());

                        if (this.timerSubscription) {
                            this.timerSubscription.unsubscribe();
                        }
                    });
            }
        });
    }

    async componentDidUnload() {
        if (this.timerSubscription) {
            this.timerSubscription.unsubscribe();
        }

        if (this.watcherSubscription) {
            this.watcherSubscription.unsubscribe();
        }
    }

    render() {
        let color: string = 'primary';
        if (this.remainingTime >= 30000 && this.remainingTime < 60000) {
            color = 'secondary';
        } else if (this.remainingTime < 30000 && this.remainingTime >= 0) {
            color = 'tertiary';
        }

        if (this.remainingText) {
            return <ion-chip color="danger" style={{background: `var(--ion-color-${color})`, color: 'var(--ion-color-light)'}}>
                <ion-label>{this.remainingText}</ion-label>
            </ion-chip>
        } else {
            return undefined;
        }
    }
}
