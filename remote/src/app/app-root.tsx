import {Component} from '@stencil/core';

import {TimerService} from './services/timer/timer.service';
import {AccelerometerService} from './services/accelerometer/accelerometer.service';

@Component({
    tag: 'app-root',
    styleUrl: 'app-root.scss'
})
export class AppRoot {

    constructor(private timerService: TimerService,
                private accelerometerService: AccelerometerService) {
        this.timerService = TimerService.getInstance();
        this.accelerometerService = AccelerometerService.getInstance();
    }

    async componentDidLoad() {
        await this.timerService.restart();
        await this.accelerometerService.init();
    }

    componentDidUnload() {
        this.timerService.destroy();
    }

    render() {
        return (
            <ion-app>
                <ion-router useHash={false}>
                    <ion-route-redirect from="/" to="/remote"/>

                    <ion-route component="app-tabs">
                        <ion-route url="/remote" component="tab-home">
                            <ion-route component="app-remote"></ion-route>
                            <ion-route url="/:room" component="app-remote"></ion-route>
                        </ion-route>

                        <ion-route url="/timer" component="tab-timer">
                            <ion-route component="app-timer"></ion-route>
                        </ion-route>

                        <ion-route url="/about" component="tab-about">
                            <ion-route component="app-about"></ion-route>
                        </ion-route>
                    </ion-route>
                </ion-router>

                <ion-router-outlet animated={true} main></ion-router-outlet>

                <ion-modal-controller></ion-modal-controller>
            </ion-app>
        );
    }
}
