import {Component, State, h, Fragment, Listen, JSX} from '@stencil/core';

import {injectJS} from '@deckdeckgo/editor';

import navStore, {NavDirection} from '../../../stores/nav.store';
import i18n from '../../../stores/i18n.store';
import tokenStore from '../../../stores/token.store';

import {AppIcon} from '../app-icon/app-icon';

import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';
import {EnvironmentAppConfig, EnvironmentDeckDeckGoConfig} from '../../../types/core/environment-config';

import {renderI18n} from '../../../utils/core/i18n.utils';

@Component({
  tag: 'app-signin',
  styleUrl: 'app-signin.scss'
})
export class AppSignIn {
  @State()
  private signInInProgress: boolean = false;

  @State()
  private signIn: JSX.IntrinsicElements;

  private appConfig: EnvironmentAppConfig = EnvironmentConfigService.getInstance().get('app');
  private deckDeckGoConfig: EnvironmentDeckDeckGoConfig | undefined = EnvironmentConfigService.getInstance().get('deckdeckgo');

  async componentDidLoad() {
    this.signInInProgress = false;

    await this.loadSignIn();

    await this.initSignIn();
  }

  private onSignInSuccess = (credentials: {uid: string | undefined; githubAccessToken: string | undefined} | undefined) => {
    this.signInInProgress = true;

    this.saveGithubCredentials(credentials);

    this.navigateRedirect();
  };

  private navigateRedirect() {
    navStore.state.nav = {
      url: '/',
      direction: NavDirection.RELOAD
    };
  }

  private saveGithubCredentials(credentials: {uid: string | undefined; githubAccessToken: string | undefined} | undefined) {
    if (!credentials) {
      return;
    }

    const {uid, githubAccessToken} = credentials;

    if (!uid || !githubAccessToken) {
      return;
    }

    tokenStore.state.token = {
      id: uid,
      data: {
        github: {
          token: githubAccessToken
        }
      }
    };
  }

  private async loadSignIn() {
    const {cloud} = this.appConfig;

    if (cloud === 'offline') {
      return;
    }

    await injectJS({
      id: `deckgo-${cloud}`,
      src: `http://localhost:3335/build/deckdeckgo-${cloud}.esm.js`,
      module: true
    });
  }

  private async initSignIn() {
    const {cloud} = this.appConfig;

    if (cloud === 'offline') {
      return;
    }

    const Element = `deckgo-${cloud}-signin`;

    this.signIn = <Element config={this.deckDeckGoConfig} signInSuccess={this.onSignInSuccess}></Element>;
  }

  async navigateBack() {
    navStore.state.nav = {
      direction: NavDirection.BACK
    };
  }

  @Listen('inProgress')
  onInProgress({detail}: CustomEvent<boolean>) {
    this.signInInProgress = detail;
  }

  render() {
    return [
      <main class="ion-padding fit">
        {this.renderBackButton()}

        {this.renderSignIn()}
      </main>
    ];
  }

  private renderSignIn() {
    return (
      <Fragment>
        {this.renderMsg()}

        {this.renderGitHub()}

        {this.signIn}
      </Fragment>
    );
  }

  private renderBackButton() {
    return (
      <ion-buttons class="back">
        <ion-button onClick={() => this.navigateBack()} color="dark" aria-label={i18n.state.core.close} disabled={this.signInInProgress}>
          <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
        </ion-button>
      </ion-buttons>
    );
  }

  private renderMsg() {
    return [
      <h1 class="ion-text-center ion-padding-start ion-padding-end">{i18n.state.sign_in.hi}</h1>,
      <p class="ion-text-center ion-padding">{i18n.state.sign_in.why}</p>
    ];
  }

  private renderGitHub() {
    const {cloud} = this.appConfig;

    if (cloud !== 'firebase') {
      return undefined;
    }

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
