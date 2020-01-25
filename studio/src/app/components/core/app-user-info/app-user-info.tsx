import {Component, Prop, State, h} from '@stencil/core';

import {Subscription} from 'rxjs';

import {AuthUser} from '../../../models/auth/auth.user';
import {User} from '../../../models/data/user';

import {ApiUser} from '../../../models/api/api.user';

import {AuthService} from '../../../services/auth/auth.service';
import {UserService} from '../../../services/data/user/user.service';
import {ApiUserService} from '../../../services/api/user/api.user.service';
import {ApiUserFactoryService} from '../../../services/api/user/api.user.factory.service';

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

  private userService: UserService;
  private userSubscription: Subscription;

  @State()
  private authUser: AuthUser;

  @State()
  private apiUser: ApiUser;

  @State()
  private name: string;

  @State()
  private photoUrl: string;

  constructor() {
    this.authService = AuthService.getInstance();
    this.apiUserService = ApiUserFactoryService.getInstance();
    this.userService = UserService.getInstance();
  }

  componentWillLoad() {
    this.authSubscription = this.authService.watch().subscribe((authUser: AuthUser) => {
      this.authUser = authUser;
    });

    this.apiUserSubscription = this.apiUserService.watch().subscribe((user: ApiUser) => {
      this.apiUser = user;
    });

    this.userSubscription = this.userService.watch().subscribe((user: User) => {
      this.name = user && user.data ? user.data.name : undefined;
      this.photoUrl = user && user.data ? user.data.photo_url : undefined;
    });
  }

  componentDidUnload() {
    if (this.authSubscription) {
      this.authSubscription.unsubscribe();
    }

    if (this.apiUserSubscription) {
      this.apiUserSubscription.unsubscribe();
    }

    if (this.userSubscription) {
      this.userSubscription.unsubscribe();
    }
  }

  render() {
    if (this.authUser) {
      return (
        <ion-grid>
          <ion-row class="ion-align-items-center">
            <ion-col size={'' + this.avatarColSize}>
              <app-avatar src={this.photoUrl} aria-hidden="true"></app-avatar>
            </ion-col>
            <ion-col size={'' + (12 - this.avatarColSize)} class="user-info">
              <ion-label>{this.name}</ion-label>
              <ion-label>{!this.authUser.anonymous && this.apiUser && this.apiUser.username ? '@' + this.apiUser.username : undefined}</ion-label>
            </ion-col>
          </ion-row>
        </ion-grid>
      );
    } else {
      return undefined;
    }
  }
}
