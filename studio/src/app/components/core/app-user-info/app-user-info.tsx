import {Component, Prop, State, h} from '@stencil/core';

import {Subscription} from 'rxjs';

import {AuthUser} from '../../../models/auth/auth.user';

import {AuthService} from '../../../services/auth/auth.service';
import {ApiUserService} from '../../../services/api/user/api.user.service';
import {ApiUser} from '../../../models/api/api.user';

@Component({
    tag: 'app-user-info',
    styleUrl: 'app-user-info.scss'
})
export class AppUserInfo {

    @Prop()
    avatarColSize: number = 4;

    private authService: AuthService;
    private authSubscription: Subscription;

    private apiUserService: ApiUserService;
    private apiUserSubscription: Subscription;

    @State()
    private authUser: AuthUser;

    @State()
    private apiUser: ApiUser;

    constructor() {
        this.authService = AuthService.getInstance();
        this.apiUserService = ApiUserService.getInstance();
    }

    componentWillLoad() {
        this.authSubscription = this.authService.watch().subscribe((authUser: AuthUser) => {
            this.authUser = authUser;
        });

        this.apiUserSubscription = this.apiUserService.watch().subscribe((user: ApiUser) => {
            this.apiUser = user;
        });
    }

    componentDidUnload() {
        if (this.authSubscription) {
            this.authSubscription.unsubscribe();
        }

        if (this.apiUserSubscription) {
            this.apiUserSubscription.unsubscribe();
        }
    }

    render() {
        if (this.authUser) {
            return <ion-grid>
                <ion-row class="ion-align-items-center">
                    <ion-col size={'' + this.avatarColSize}><app-avatar src={this.authUser.photo_url}></app-avatar></ion-col>
                    <ion-col size={'' + (12 - this.avatarColSize)} class="user-info">
                        <ion-label>{this.authUser.name}</ion-label>
                        <ion-label>{!this.authUser.anonymous && this.apiUser && this.apiUser.username ? '@' + this.apiUser.username : undefined}</ion-label>
                    </ion-col>
                </ion-row>
            </ion-grid>
        } else {
            return undefined;
        }
    }

}
