import {Component, State} from '@stencil/core';
import {filter, take} from 'rxjs/operators';

import {AuthUser} from '../../../models/auth-user';

import {AuthService} from '../../../services/api/auth/auth.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';

@Component({
    tag: 'app-settings',
    styleUrl: 'app-settings.scss'
})
export class AppHome {

    @State()
    private authUser: AuthUser;

    private authService: AuthService;

    private navService: NavService;

    constructor() {
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();
    }

    componentWillLoad() {
        this.authService.watch().pipe(
            filter((authUser: AuthUser) => authUser !== null && authUser !== undefined),
            take(1)).subscribe(async (authUser: AuthUser) => {
                this.authUser = authUser;
        });
    }

    private async signIn() {
        this.navService.navigate({
            url: '/signin' + (window && window.location ? window.location.pathname : ''),
            direction: NavDirection.FORWARD
        });
    }

    render() {
        return [
            <app-navigation></app-navigation>,
            <ion-content class="ion-padding fullscreen-padding">
                <main padding>
                    {this.renderGuardedContent()}
                </main>
            </ion-content>
        ];
    }

    private renderGuardedContent() {
        if (!this.authUser) {
            return this.renderNotLoggedInContent();
        } else{
            return <h1>Settings</h1>
        }
    }

    private renderNotLoggedInContent() {
        return [
            <h1>Oh, hi! Good to have you.</h1>,
            <p class="ion-padding-top"><ion-anchor onClick={() => this.signIn()}>Sign in</ion-anchor> to access your settings.</p>
        ]
    }
}
