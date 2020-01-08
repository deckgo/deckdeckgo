import {Component, h, State} from '@stencil/core';

import {AuthUser} from '../../../models/auth/auth.user';

import {AuthService} from '../../../services/auth/auth.service';

@Component({
    tag: 'app-home'
})
export class AppHome {

    private authService: AuthService;

    @State()
    private landing: boolean | undefined = undefined;

    constructor() {
        this.authService = AuthService.getInstance();
    }

    async componentWillLoad() {
        const localUser: AuthUser = await this.authService.getLocalAuthUser();
        this.landing = localUser === undefined || localUser.anonymous;
     }

    render() {
        if (this.landing === undefined) {
            return undefined;
        }

        return [
            <app-navigation presentation={true}></app-navigation>,
            this.renderContent()
        ];
    }

    private renderContent() {
        if (this.landing) {
            return <ion-content>
                <app-landing></app-landing>
            </ion-content>
        } else {
            return <ion-content class="ion-padding">
                <app-feed></app-feed>
            </ion-content>
        }
    }
}
