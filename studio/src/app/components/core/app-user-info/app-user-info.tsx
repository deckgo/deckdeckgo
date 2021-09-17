import {Component, Prop, h} from '@stencil/core';

import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';

@Component({
  tag: 'app-user-info',
  styleUrl: 'app-user-info.scss'
})
export class AppUserInfo {
  @Prop()
  displayAvatar: boolean = false;

  render() {
    if (!authStore.state.authUser) {
      return undefined;
    }

    return this.renderUser();
  }

  private renderUser() {
    return (
      <ion-grid class={this.displayAvatar ? 'avatar' : ''}>
        <ion-row class="ion-align-items-center">
          {this.renderAvatar()}

          <ion-col size={this.displayAvatar ? '9' : '12'} class="user-info">
            <ion-label>{userStore.state.name}</ion-label>
            <ion-label>{this.renderUsername()}</ion-label>
          </ion-col>
        </ion-row>
      </ion-grid>
    );
  }

  private renderAvatar() {
    if (!this.displayAvatar) {
      return undefined;
    }

    return (
      <ion-col size="3">
        <app-avatar src={userStore.state.photoUrl} aria-hidden="true"></app-avatar>
      </ion-col>
    );
  }

  private renderUsername() {
    const username: string | undefined = userStore.state.user?.data?.username;

    if (!username || authStore.state.anonymous) {
      return undefined;
    }

    return <ion-label>{`@${username}`}</ion-label>;
  }
}
