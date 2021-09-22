import {Component, Event, h, Fragment, EventEmitter} from '@stencil/core';

import {injectJS} from '@deckdeckgo/editor';

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

  private signInSuccess = ({uid, githubAccessToken}: {uid: string | undefined; githubAccessToken: string | undefined}) => {
    this.inProgress.emit(true);

    this.saveGithubCredentials({uid, githubAccessToken});

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

  private navigateRedirect() {
    navStore.state.nav = {
      url: '/',
      direction: NavDirection.RELOAD
    };
  }

  private saveGithubCredentials({uid, githubAccessToken}: {uid: string | undefined; githubAccessToken: string | undefined}) {
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

  render() {
    return (
      <Fragment>
        {this.renderMsg()}

        {this.renderGitHub()}

        <deckgo-firebase-signin app-url={this.deckDeckGoConfig.appUrl} signInSuccess={this.signInSuccess}></deckgo-firebase-signin>
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
