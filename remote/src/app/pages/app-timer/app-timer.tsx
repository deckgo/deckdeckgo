import {Component, Element, State} from '@stencil/core';
import {DatetimeChangeEventDetail} from '@ionic/core';

import {BehaviorSubject, Subscription} from 'rxjs';

import {Comparator} from '../../services/utils/utils';

import {TimerInterval, TimerService} from '../../services/timer/timer.service';
import {NotificationService} from '../../services/notification/notification.service';

@Component({
    tag: 'app-timer',
    styleUrl: 'app-timer.scss'
})
export class AppTimer {

    @Element() el: HTMLElement;

    private watcherSubscription: Subscription;
    private timerSubscription: Subscription;

    @State()
    private timerRemaining: number;

    @State()
    private timerLength: number;

    @State()
    private timerRunning: boolean;

    @State()
    private timerPause: boolean = false;

    constructor(private timerService: TimerService,
                private notificationService: NotificationService) {
        this.timerService = TimerService.getInstance();
        this.notificationService = NotificationService.getInstance();
    }

    async componentDidLoad() {
        this.notificationService.init();

        this.watchTimer();

        this.timerRunning = await this.timerService.isTimerStarted();

        if (this.timerRunning) {
            await this.toggleFabActivated();
        }
    }

    async componentDidUnload() {
        if (this.timerSubscription) {
            this.timerSubscription.unsubscribe();
        }

        if (this.watcherSubscription) {
            this.watcherSubscription.unsubscribe();
        }
    }

    private watchTimer() {
        this.watcherSubscription = this.timerService.watch().subscribe((timer: BehaviorSubject<TimerInterval>) => {
            if (timer) {
                this.timerSubscription = timer.subscribe((interval: TimerInterval) => {
                        if (interval) {
                            this.timerRemaining = interval.timerRemaining;
                            this.timerLength = interval.timerLength;
                        }
                    },
                    (_err) => {
                        // Do nothing
                    }, async () => {
                        await this.clearStopwatch();
                    });
            }
        });
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

    private async clearStopwatch() {
        if (this.timerSubscription) {
            this.timerSubscription.unsubscribe();
        }

        // Reset timerRemaining
        this.timerRemaining = 0;
        this.timerLength = 0;
        this.timerRunning = false;

        await this.timerService.clearEndAt();

        await this.resetDatetime();
    }

    private async resetDatetime() {
        await this.deactivatedFab();

        const datetimeElement: HTMLIonDatetimeElement = this.el.querySelector('ion-datetime');

        if (datetimeElement) {
            datetimeElement.value = null;
        }
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-title text-uppercase>DeckDeckGo</ion-title>
                </ion-toolbar>
            </ion-header>,

            <ion-content padding>
                {this.renderContent()}
                {this.renderActions()}
                <ion-datetime display-format="HH:mm" pickerOptions={{backdropDismiss: false}}
                              onIonCancel={() => this.toggleFabActivated()}
                              onIonChange={(e: CustomEvent<DatetimeChangeEventDetail>) => this.initTimerLengthAndStartTimer(e)}></ion-datetime>
            </ion-content>
        ];
    }

    private renderContent() {
        if (this.timerRemaining >= 0) {
            return <div class="content">
                <app-stopwatch length={this.timerLength} remaining={this.timerRemaining}></app-stopwatch>
            </div>
        } else {
            return <h1 padding-bottom>The DeckDeckGo remote timer</h1>;
        }
    }

    private renderActions() {
        if (this.timerRunning === null || (this.timerRunning && this.timerRemaining === null)) {
            return undefined;
        }

        return <ion-fab vertical="bottom" horizontal="end" slot="fixed" onClick={(e: UIEvent) => e.stopPropagation()}>
            <ion-fab-button onClick={() => this.startStopAction()}>
                <ion-icon name="stopwatch"></ion-icon>
            </ion-fab-button>
            <ion-fab-list side="start">
                {this.renderActionsPause()}
            </ion-fab-list>
        </ion-fab>
    }

    private renderActionsPause() {
        if (this.timerPause) {
            return <ion-fab-button color="medium" onClick={(e: UIEvent) => this.pauseTimer(e, false)}>
                <ion-icon name="play"></ion-icon>
            </ion-fab-button>
        } else {
            return <ion-fab-button color="medium" onClick={(e: UIEvent) => this.pauseTimer(e, true)}>
                <ion-icon name="pause"></ion-icon>
            </ion-fab-button>
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

            const values: string[] = $event.detail.value.split(':');

            if (Comparator.isEmpty(values) || values.length < 2) {
                resolve();
                return;
            }

            const hours: number = parseInt(values[0], 0);
            const minutes: number = parseInt(values[1], 0);

            if (hours <= 0 && minutes <= 0) {
                resolve();
                return;
            }

            const length: number = (minutes * 60 * 1000) + (hours * 60 * 60 * 1000);

            await this.startTimer(length);

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

            if (ionFabElement.activated) {
                await this.openDatetime();
            } else {
                await this.stopTimer();
            }

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
}
