import {Component, Prop, State, h} from '@stencil/core';

import {Subscription} from 'rxjs';
import {filter} from 'rxjs/operators';

import {AuthUser} from '../../../models/data/auth-user';
import {ApiUser} from '../../../models/api/api.user';

import {ApiUserService} from '../../../services/api/user/api.user.service';
import {AuthService} from '../../../services/data/auth/auth.service';

@Component({
    tag: 'app-user-info',
    styleUrl: 'app-user-info.scss'
})
export class AppUserInfo {

    @Prop()
    avatarColSize: number = 4;

    private authService: AuthService;
    private authSubscription: Subscription;

    private userService: ApiUserService;
    private userSubscription: Subscription;

    @State()
    private authUser: AuthUser;

    @State()
    private user: ApiUser;

    constructor() {
        this.authService = AuthService.getInstance();
        this.userService = ApiUserService.getInstance();
    }

    componentWillLoad() {
        this.authSubscription = this.authService.watch().subscribe((authUser: AuthUser) => {
            this.authUser = authUser;
        });

        this.userSubscription = this.userService.watch().pipe(
            filter((user: ApiUser) => user && !user.anonymous)).subscribe(async (user: ApiUser) => {
            this.user = user;
        });
    }

    componentDidUnload() {
        if (this.authSubscription) {
            this.authSubscription.unsubscribe();
        }

        if (this.userSubscription) {
            this.userSubscription.unsubscribe();
        }
    }

    render() {
        if (this.authUser) {
            return <ion-grid>
                <ion-row class="ion-align-items-center">
                    <ion-col size={'' + this.avatarColSize}><app-avatar src={this.authUser.photo_url}></app-avatar></ion-col>
                    <ion-col size={'' + (12 - this.avatarColSize)} class="user-info">
                        <ion-label>{this.authUser.name}</ion-label>
                        <ion-label>{this.user && this.user.username ? '@' + this.user.username : undefined}</ion-label>
                    </ion-col>
                </ion-row>
            </ion-grid>
        } else {
            return undefined;
        }
    }

}
