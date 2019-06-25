import {Component, Event, EventEmitter, Prop, State, h} from '@stencil/core';

import {Subscription} from 'rxjs';

import {AuthUser} from '../../../models/auth-user';

import {Utils} from '../../../utils/core/utils';
import {IonControllerUtils} from '../../../utils/core/ion-controller-utils';

import {AuthService} from '../../../services/data/auth/auth.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';

@Component({
    tag: 'app-navigation-actions',
    styleUrl: 'app-navigation-actions.scss',
    shadow: false
})
export class AppNavigationActions {

    @Prop() signIn: boolean = true;
    @Prop() presentation: boolean = false;
    @Prop() publish: boolean = false;

    private authService: AuthService;
    private subscription: Subscription;

    private navService: NavService;

    @State()
    private authUser: AuthUser;

    @Event() private actionPublish: EventEmitter<void>;

    constructor() {
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();
    }

    componentWillLoad() {
        this.subscription = this.authService.watch().subscribe((authUser: AuthUser) => {
            this.authUser = authUser;
        });
    }

    componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
        }
    }

    private async openMenu($event: UIEvent) {
        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
            component: 'app-user-menu',
            event: $event,
            mode: 'ios'
        });

        await popover.present();
    }

    private async navigateSignIn() {
        this.navService.navigate({
            url: '/signin' + (window && window.location ? window.location.pathname : ''),
            direction: NavDirection.FORWARD
        });
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
        if (Utils.isLoggedIn(this.authUser) || !this.signIn) {
            return undefined;
        } else if (this.presentation || this.publish) {
            return <a class="signin ion-padding-start ion-padding-end" onClick={() => this.navigateSignIn()}>
                <ion-label>Sign in</ion-label>
            </a>;
        }
    }

    private renderLoggedIn() {
        if (Utils.isLoggedIn(this.authUser)) {
            return <a class="ion-padding-end" onClick={(e: UIEvent) => this.openMenu(e)}>
                <app-avatar src={this.authUser.photo_url}></app-avatar>
            </a>;
        } else {
            return undefined;
        }
    }

    private renderPresentationButton() {
        if (this.presentation) {
            return <ion-button class="presentation ion-padding-end" shape="round" href="/editor" routerDirection="root" mode="md">
                <ion-label class="ion-text-uppercase">Write a presentation</ion-label>
            </ion-button>;
        } else {
            return null;
        }
    }

    private renderPublishButton() {
        if (this.publish) {
            return <ion-button class="publish ion-padding-end" shape="round" onClick={() => this.actionPublish.emit()} mode="md">
                <ion-label class="ion-text-uppercase">Ready to share?</ion-label>
            </ion-button>;
        } else {
            return null;
        }
    }

}
