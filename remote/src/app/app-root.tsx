import {Build, Component, h} from '@stencil/core';

import {Subscription} from 'rxjs';

import {TimerService} from './services/timer/timer.service';
import {AccelerometerService} from './services/accelerometer/accelerometer.service';
import {ThemeService} from './services/theme/theme.service';

@Component({
    tag: 'app-root',
    styleUrl: 'app-root.scss'
})
export class AppRoot {

    private timerService: TimerService;
    private accelerometerService: AccelerometerService;

    private themeSubscription: Subscription;
    private themeService: ThemeService;

    private domBodyClassList: DOMTokenList = document.body.classList;

    constructor() {
        this.timerService = TimerService.getInstance();
        this.accelerometerService = AccelerometerService.getInstance();
        this.themeService = ThemeService.getInstance();
    }

    async componentWillLoad() {
        this.themeSubscription = this.themeService.watch().subscribe((dark: boolean) => {
            this.updateDarkModePreferences(dark);
        });

        await this.themeService.initDarkModePreference();
    }

    async componentDidLoad() {
        await this.timerService.restart();

        if (Build.isBrowser) {
            await this.accelerometerService.init();
        }
    }

    componentDidUnload() {
        this.timerService.destroy();

        if (this.themeSubscription) {
            this.themeSubscription.unsubscribe();
        }
    }

    private updateDarkModePreferences(dark: boolean) {
        dark ?
            this.domBodyClassList.add('dark') :
            this.domBodyClassList.remove('dark');
    }

    render() {
        return (
            <ion-app>
                <ion-router useHash={false}>
                    <ion-route url="/" component="app-remote"></ion-route>
                    <ion-route url="/remote" component="app-remote"></ion-route>
                    <ion-route url="/remote/:room" component="app-remote"></ion-route>

                    <ion-route url="/timer" component="app-timer"></ion-route>

                    <ion-route url="/settings" component="app-settings"></ion-route>

                    <ion-route url="/about" component="app-about"></ion-route>
                </ion-router>

                <ion-menu side="start" type="push" swipeGesture={false} content-id="menu-content">
                    <ion-header>
                        <ion-toolbar>
                            <ion-title slot="start" class="ion-no-padding ion-margin-start ion-margin-end">
                                <a href="/">
                                    <app-logo></app-logo>
                                    <span>DeckDeckGo</span>
                                </a>
                            </ion-title>
                        </ion-toolbar>
                    </ion-header>

                    <ion-content>
                        <ion-menu-toggle autoHide={false}>
                            <ion-item detail={false} href="/" routerDirection="forward"><ion-label>Remote</ion-label></ion-item>
                            <ion-item detail={false} href="/timer" routerDirection="forward"><ion-label>Timer</ion-label></ion-item>
                            <ion-item detail={false} href="/settings" routerDirection="forward"><ion-label>Settings</ion-label></ion-item>
                            <ion-item detail={false} href="/about" routerDirection="forward"><ion-label>About</ion-label></ion-item>
                        </ion-menu-toggle>
                    </ion-content>
                </ion-menu>

                <ion-nav id="menu-content"/>
                
                <ion-modal-controller></ion-modal-controller>
                
            </ion-app>
        );
    }
}
