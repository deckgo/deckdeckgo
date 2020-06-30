import {Component, Prop, h} from '@stencil/core';

import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';
import apiUserStore from '../../../stores/api.user.store';

@Component({
  tag: 'app-user-info',
  styleUrl: 'app-user-info.scss',
})
export class AppUserInfo {
  @Prop()
  avatarColSize: number = 4;

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
              <ion-label>
                {!authStore.state.authUser.anonymous && apiUserStore.state.apiUser && apiUserStore.state.apiUser.username
                  ? '@' + apiUserStore.state.apiUser.username
                  : undefined}
              </ion-label>
            </ion-col>
          </ion-row>
        </ion-grid>
      );
    } else {
      return undefined;
    }
  }
}
