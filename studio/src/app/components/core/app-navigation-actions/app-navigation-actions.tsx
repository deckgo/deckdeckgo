import {Component, Prop, State} from '@stencil/core';

import {Subscription} from 'rxjs';

import {User} from '../../../models/user';

import {AuthService} from '../../../services/auth/auth.service';

@Component({
    tag: 'app-navigation-actions',
    styleUrl: 'app-navigation-actions.scss',
    shadow: false
})
export class AppNavigationActions {

    @Prop({connect: 'ion-modal-controller'}) modalController: HTMLIonModalControllerElement;
    @Prop({connect: 'ion-popover-controller'}) popoverController: HTMLIonPopoverControllerElement;

    @Prop() presentation: boolean = false;
    @Prop() publish: boolean = false;

    private authService: AuthService;
    private subscription: Subscription;

    @State()
    private user: User;

    constructor() {
        this.authService = AuthService.getInstance();
    }

    componentWillLoad() {
        this.subscription = this.authService.watch().subscribe((user: User) => {
            this.user = user;
        });
    }

    componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
        }
    }

    private async openMenu($event: UIEvent) {
        const popover: HTMLIonPopoverElement = await this.popoverController.create({
            component: 'app-user-menu',
            event: $event,
            mode: 'ios'
        });

        await popover.present();
    }

    private async signIn() {
        const modal: HTMLIonModalElement = await this.modalController.create({
            component: 'app-login'
        });

        await modal.present();
    }

    render() {
        return <div>
            {this.renderSignIn()}
            {this.renderPresentationButton()}
            {this.renderPublishButton()}
            {this.renderLoggedIn()}
        </div>;
    }

    private renderSignIn() {
        if (this.user) {
            return undefined;
        } else {
            return <a padding-start padding-end class="signin" onClick={() => this.signIn()}>
                <ion-label>Sign in</ion-label>
            </a>;
        }
    }

    private renderLoggedIn() {
        if (this.user) {
            return <a padding-end onClick={(e: UIEvent) => this.openMenu(e)}>
                <app-avatar src={this.user.photo_url}></app-avatar>
            </a>;
        } else {
            return undefined;
        }
    }

    private renderPresentationButton() {
        if (this.presentation) {
            return <ion-button class="presentation" shape="round" href="/editor" routerDirection="forward" padding-end>
                <ion-label text-uppercase>Write a presentation</ion-label>
            </ion-button>;
        } else {
            return null;
        }
    }

    private renderPublishButton() {
        if (this.publish) {
            return <ion-button class="publish" shape="round" href="/editor" routerDirection="forward" padding-end>
                <ion-label text-uppercase>Ready to publish?</ion-label>
            </ion-button>;
        } else {
            return null;
        }
    }

}
