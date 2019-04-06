import {Component, State} from '@stencil/core';

import {Subscription} from 'rxjs';

import {AuthUser} from '../../../models/auth-user';

import {Utils} from '../../../utils/utils';

import {AuthService} from '../../../services/auth/auth.service';
import {NavDirection, NavService} from '../../../services/nav/nav.service';

@Component({
    tag: 'app-menu-user',
    styleUrl: 'app-menu-user.scss',
    shadow: false
})
export class AppMenuUser {

    private authService: AuthService;
    private subscription: Subscription;

    private navService: NavService;

    @State()
    private authUser: AuthUser;

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

    private async signIn() {
        this.navService.navigate({
            url: '/signin',
            direction: NavDirection.FORWARD
        });
    }

    private async signOut() {
        await this.authService.signOut();

        this.navService.navigate({
            url: '/',
            direction: NavDirection.ROOT
        });
    }

    render() {
        return <ion-list>
            {this.renderUser()}

            <ion-item-divider>
                <ion-label>Presentations</ion-label>
                <ion-button size="small" slot="end" shape="round" margin-end href="/editor"
                            routerDirection="forward" class="new">
                    <ion-icon name="book" slot="start"></ion-icon>
                    <ion-label>New</ion-label>
                </ion-button>
            </ion-item-divider>

            {this.renderPresentations()}

            {this.renderSignOut()}

        </ion-list>;
    }

    private renderUser() {
        if (Utils.isLoggedIn(this.authUser)) {
            return <ion-item class="user">
                <app-avatar slot="start" src={this.authUser.photo_url}></app-avatar>
                <ion-label>{this.authUser.name}</ion-label>
            </ion-item>;
        } else {
            return <ion-item class="user"></ion-item>;
        }
    }

    // TODO: deck.service get userId

    // TODO: modify editor route to load deck slides with params

    private renderPresentations() {
        if (Utils.isLoggedIn(this.authUser)) {
            return [
                <ion-item href="/editor" routerDirection="forward">
                    <ion-icon name="book" slot="start"></ion-icon>
                    <ion-label>Presentation A</ion-label>
                </ion-item>,

                <ion-item href="/editor" routerDirection="forward">
                    <ion-icon name="book" slot="start"></ion-icon>
                    <ion-label>Presentation B</ion-label>
                </ion-item>];
        } else {
            return <ion-item button onClick={() => this.signIn()}>
                <ion-icon name="log-in" slot="start"></ion-icon>
                <ion-label>Sign in</ion-label>
            </ion-item>;
        }
    }

    private renderSignOut() {
        if (Utils.isLoggedIn(this.authUser)) {
            return <ion-item button class="signout" onClick={() => this.signOut()}>
                <ion-icon name="log-out" slot="start"></ion-icon>
                <ion-label>Sign out</ion-label>
            </ion-item>;
        } else {
            return undefined;
        }
    }

}
