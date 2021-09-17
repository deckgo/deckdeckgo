import {Component, Event, h, Fragment, EventEmitter} from '@stencil/core';

import {injectJS} from '@deckdeckgo/editor';

import type {UserCredential, OAuthCredential} from '@firebase/auth-types';

import navStore, {NavDirection} from '../../../../stores/nav.store';
import tokenStore from '../../../../stores/token.store';
import i18n from '../../../../stores/i18n.store';

import {renderI18n} from '../../../../utils/core/i18n.utils';

import {EnvironmentDeckDeckGoConfig} from '../../../../types/core/environment-config';

import {EnvironmentConfigService} from '../../../../services/environment/environment-config.service';

import {AppIcon} from '../../app-icon/app-icon';

@Component({
  tag: 'app-signin-firebase'
})
export class AppSignInFirebase {
  @Event()
  inProgress: EventEmitter<boolean>;

  private deckDeckGoConfig: EnvironmentDeckDeckGoConfig | undefined = EnvironmentConfigService.getInstance().get('deckdeckgo');

  private signInSuccessWithAuthResult = (authResult, _redirectUrl) => {
    this.inProgress.emit(true);

    this.saveGithubCredentials(authResult);

    this.navigateRedirect();

    return false;
  };

  async componentDidLoad() {
    this.inProgress.emit(false);

    await injectJS({
      id: 'deckgo-firebase',
      src: 'http://localhost:3335/build/deckdeckgo-firebase.esm.js',
      module: true
    });
  }

  private navigateRedirect(redirectStatus: 'success' | 'failure' = 'success') {
    const redirectUrl: string = localStorage.getItem('deckdeckgo_redirect');

    localStorage.removeItem('deckdeckgo_redirect');

    this.navigateRoot(redirectUrl, redirectStatus, NavDirection.RELOAD);
  }

  private navigateRoot(redirectUrl: string, redirectStatus: 'success' | 'failure', direction: NavDirection) {
    // TODO: That's ugly
    const url: string = !redirectUrl || redirectUrl.trim() === '' || redirectUrl.trim() === '/' ? '/' : '/' + redirectUrl + '/';

    // Do not push a new page but reload as we might later face a DOM with contains two firebaseui which would not work
    navStore.state.nav = {
      url: url + `?signin=${redirectStatus}`,
      direction
    };
  }

  private saveGithubCredentials(userCred: UserCredential) {
    if (!userCred || !userCred.user || !userCred.user.uid) {
      return;
    }

    if (!userCred.credential || userCred.credential.providerId !== 'github.com' || !(userCred.credential as OAuthCredential).accessToken) {
      return;
    }

    tokenStore.state.token = {
      id: userCred.user.uid,
      data: {
        github: {
          token: (userCred.credential as OAuthCredential).accessToken
        }
      }
    };
  }

  render() {
    return (
      <Fragment>
        {this.renderMsg()}

        {this.renderGitHub()}

        <deckgo-firebase-signin
          app-url={this.deckDeckGoConfig.appUrl}
          signInSuccessWithAuthResult={this.signInSuccessWithAuthResult}></deckgo-firebase-signin>
      </Fragment>
    );
  }

  private renderMsg() {
    return [
      <h1 class="ion-text-center ion-padding-start ion-padding-end">{i18n.state.sign_in.hi}</h1>,
      <p class="ion-text-center ion-padding">{i18n.state.sign_in.why}</p>
    ];
  }

  private renderGitHub() {
    return (
      <p class="ion-text-center ion-padding-start ion-padding-end ion-padding-bottom">
        {renderI18n(i18n.state.sign_in.additionally, {
          placeholder: '{0}',
          value: <AppIcon name="github" ariaLabel="" ariaHidden={true}></AppIcon>
        })}
      </p>
    );
  }
}
