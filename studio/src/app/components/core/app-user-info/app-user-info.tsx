import {Component, Prop, State, h} from '@stencil/core';

import {Subscription} from 'rxjs';

import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';

import {ApiUser} from '../../../models/api/api.user';

import {ApiUserService} from '../../../services/api/user/api.user.service';
import {ApiUserFactoryService} from '../../../services/api/user/api.user.factory.service';

@Component({
  tag: 'app-user-info',
  styleUrl: 'app-user-info.scss',
})
export class AppUserInfo {
  @Prop()
  avatarColSize: number = 4;

  private apiUserService: ApiUserService;
  private apiUserSubscription: Subscription;

  @State()
  private apiUser: ApiUser;

  constructor() {
    this.apiUserService = ApiUserFactoryService.getInstance();
  }

  componentWillLoad() {
    this.apiUserSubscription = this.apiUserService.watch().subscribe((user: ApiUser) => {
      this.apiUser = user;
    });
  }

  componentDidUnload() {
    if (this.apiUserSubscription) {
      this.apiUserSubscription.unsubscribe();
    }
  }

  render() {
    if (authStore.state.authUser) {
      return (
        <ion-grid>
          <ion-row class="ion-align-items-center">
            <ion-col size={'' + this.avatarColSize}>
              <app-avatar src={userStore.state.photoUrl} aria-hidden="true"></app-avatar>
            </ion-col>
            <ion-col size={'' + (12 - this.avatarColSize)} class="user-info">
              <ion-label>{userStore.state.name}</ion-label>
              <ion-label>{!authStore.state.authUser.anonymous && this.apiUser && this.apiUser.username ? '@' + this.apiUser.username : undefined}</ion-label>
            </ion-col>
          </ion-row>
        </ion-grid>
      );
    } else {
      return undefined;
    }
  }
}
