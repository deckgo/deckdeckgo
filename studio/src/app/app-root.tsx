import {Component, Prop} from '@stencil/core';

import {Subscription} from 'rxjs';

import {ErrorService} from './services/error/error.service';

import {AuthService} from './services/auth/auth.service';

@Component({
    tag: 'app-root',
    styleUrl: 'app-root.scss'
})
export class AppRoot {

    @Prop({connect: 'ion-menu-controller'}) lazyMenuController!: HTMLIonMenuControllerElement;
    @Prop({connect: 'ion-toast-controller'}) toastController: HTMLIonToastControllerElement;

    private subscription: Subscription;

    private errorService: ErrorService;

    private authService: AuthService;

    constructor() {
        this.errorService = ErrorService.getInstance();
        this.authService = AuthService.getInstance();
    }

    async componentWillLoad() {
        await this.authService.init();
    }

    async componentDidLoad() {
        this.subscription = this.errorService.watch().subscribe(async (error: string) => {
            await this.toastError(error);
        });
    }

    async componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
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

    render() {
        return ([
            <ion-app>
                <ion-router useHash={false}>
                    <ion-route url="/" component="app-home"/>
                    <ion-route url="/editor" component="app-editor"/>

                    <ion-route url="/about" component="app-about"/>
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
                                <ion-list>
                                    <ion-item class="user">
                                        <app-avatar slot="start" src="https://pbs.twimg.com/profile_images/941274539979366400/bTKGkd-O_400x400.jpg"></app-avatar>
                                        <ion-label>David Dal Busco</ion-label>
                                    </ion-item>

                                    <ion-item-divider>
                                        <ion-label>Presentations</ion-label>
                                        <ion-button size="small" slot="end" shape="round" margin-end href="/editor" routerDirection="forward">
                                            <ion-icon name="book" slot="start"></ion-icon>
                                            <ion-label>New</ion-label>
                                        </ion-button>
                                    </ion-item-divider>

                                    <ion-item href="/editor" routerDirection="forward">
                                        <ion-icon name="book" slot="start"></ion-icon>
                                        <ion-label>Presentation A</ion-label>
                                    </ion-item>

                                    <ion-item href="/editor" routerDirection="forward">
                                        <ion-icon name="book" slot="start"></ion-icon>
                                        <ion-label>Presentation B</ion-label>
                                    </ion-item>
                                </ion-list>

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
