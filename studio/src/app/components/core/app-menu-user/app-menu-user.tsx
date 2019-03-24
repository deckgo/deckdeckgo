import {Component, State} from '@stencil/core';

import {Subscription} from 'rxjs';

import {User} from '../../../models/user';

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
    private user: User;

    constructor() {
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();
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

    private async signIn() {
        this.navService.navigate({
            url: '/login',
            direction: NavDirection.FORWARD
        });
    }

    private async signOut() {
        await this.authService.logout();
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
        if (Utils.isLoggedIn(this.user)) {
            return <ion-item class="user">
                <app-avatar slot="start" src={this.user.photo_url}></app-avatar>
                <ion-label>{this.user.name}</ion-label>
            </ion-item>;
        } else {
            return <ion-item class="user"></ion-item>;
        }
    }

    private renderPresentations() {
        if (Utils.isLoggedIn(this.user)) {
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
        if (Utils.isLoggedIn(this.user)) {
            return <ion-item button class="signout" onClick={() => this.signOut()}>
                <ion-icon name="log-out" slot="start"></ion-icon>
                <ion-label>Sign out</ion-label>
            </ion-item>;
        } else {
            return undefined;
        }
    }

}
