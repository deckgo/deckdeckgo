import {Component, Element, Prop} from '@stencil/core';

import {Subscription} from 'rxjs';

import {ErrorService} from './services/error/error.service';
import {AuthService} from './services/auth/auth.service';
import {NavDirection, NavParams, NavService} from './services/nav/nav.service';

@Component({
    tag: 'app-root',
    styleUrl: 'app-root.scss'
})
export class AppRoot {

    @Element() el: HTMLElement;

    @Prop({connect: 'ion-menu-controller'}) lazyMenuController!: HTMLIonMenuControllerElement;
    @Prop({connect: 'ion-toast-controller'}) toastController: HTMLIonToastControllerElement;

    private errorSubscription: Subscription;
    private errorService: ErrorService;

    private authService: AuthService;

    private navSubscription: Subscription;
    private navService: NavService;

    constructor() {
        this.errorService = ErrorService.getInstance();
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();
    }

    async componentWillLoad() {
        await this.authService.init();
    }

    async componentDidLoad() {
        this.errorSubscription = this.errorService.watch().subscribe(async (error: string) => {
            await this.toastError(error);
        });

        this.navSubscription = this.navService.watch().subscribe(async (params: NavParams) => {
            await this.navigate(params);
        });
    }

    async componentDidUnload() {
        if (this.errorSubscription) {
            this.errorSubscription.unsubscribe();
        }

        if (this.navSubscription) {
            this.navSubscription.unsubscribe();
        }
    }

    private async toastError(error: string) {
        const popover: HTMLIonToastElement = await this.toastController.create({
            message: error,
            showCloseButton: true,
            position: 'top',
            closeButtonText: 'Ok, sh*t happens',
            color: 'danger',
            duration: 6000
        });

        await popover.present();
    }

    private async navigate(params: NavParams) {
        if (!params) {
            return;
        }

        const router: HTMLIonRouterElement = this.el.querySelector('ion-router');

        if (!router) {
            return;
        }

        if (params.direction === NavDirection.ROOT) {
            window.location.assign(params.url);
        } else {
            await router.push(params.url);
        }
    }

    render() {
        return ([
            <ion-app>
                <ion-router useHash={false}>
                    <ion-route url="/" component="app-home"/>

                    <ion-route url="/editor" component="app-editor"/>

                    <ion-route url="/signin" component="app-signin"/>
                    <ion-route url="/signin/:redirect" component="app-signin"/>

                    <ion-route url="/about" component="app-about"/>
                    <ion-route url="/team" component="app-team"/>
                    <ion-route url="/opensource" component="app-opensource"/>
                    <ion-route url="/privacy" component="app-privacy"/>
                    <ion-route url="/terms" component="app-terms"/>
                    <ion-route url="/contact" component="app-contact"/>
                </ion-router>

                <ion-split-pane when="lg">
                    <ion-menu id="ion-menu" side="start" type="push" swipeGesture={false}>
                        <app-navigation logo={true} menuToggle={false} user={false}></app-navigation>
                        <ion-content>
                            <ion-menu-toggle autoHide={false}>
                                <app-menu-user></app-menu-user>

                                <app-footer></app-footer>
                            </ion-menu-toggle>
                        </ion-content>
                    </ion-menu>

                    <ion-nav main/>
                </ion-split-pane>
            </ion-app>,
            <ion-menu-controller></ion-menu-controller>
        ]);
    }
}
