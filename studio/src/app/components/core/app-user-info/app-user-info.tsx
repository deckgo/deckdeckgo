import {Component, Prop, h} from '@stencil/core';

import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';
import apiUserStore from '../../../stores/api.user.store';

@Component({
  tag: 'app-user-info',
  styleUrl: 'app-user-info.scss'
})
export class AppUserInfo {
  @Prop()
  displayAvatar: boolean = false;

  render() {
    if (authStore.state.authUser) {
      return (
        <ion-grid class={this.displayAvatar ? 'avatar' : ''}>
          <ion-row class="ion-align-items-center">
            {this.displayAvatar ? (
              <ion-col size="3">
                <app-avatar src={userStore.state.photoUrl} aria-hidden="true"></app-avatar>
              </ion-col>
            ) : undefined}

            <ion-col size={this.displayAvatar ? '9' : '12'} class="user-info">
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
