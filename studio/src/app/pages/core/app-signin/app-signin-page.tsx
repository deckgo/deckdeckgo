import {Component, Fragment, h, State} from '@stencil/core';

import {AuthUser} from '@deckdeckgo/editor';

import authStore from '../../../stores/auth.store';

@Component({
  tag: 'app-signin-page'
})
export class AppSigninPage {
  @State()
  private signin: boolean;

  private destroyListener;

  async componentWillLoad() {
    this.destroyListener = authStore.onChange('authUser', async (authUser: AuthUser | null) => {
      if (!authUser) {
        this.signin = true;
      }
    });

    await this.initSignedIn();
  }

  private async initSignedIn() {
    this.signin = authStore.state.authUser === undefined || authStore.state.anonymous;
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  render() {
    if (this.signin) {
      return (
        <Fragment>
          <app-navigation actions={false}></app-navigation>
          <ion-content class="ion-padding fullscreen-padding">
            <app-signin></app-signin>
          </ion-content>
        </Fragment>
      );
    }

    return (
      <Fragment>
        <app-navigation></app-navigation>
        <ion-content class="ion-padding">
          <app-dashboard></app-dashboard>
        </ion-content>
      </Fragment>
    );
  }
}
