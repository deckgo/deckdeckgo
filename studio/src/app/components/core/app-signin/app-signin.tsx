import {Component, Element, Prop, State, Watch, h} from '@stencil/core';

import firebase from '@firebase/app';
import '@firebase/auth';
import {UserCredential, OAuthCredential} from '@firebase/auth-types';

import navStore, {NavDirection} from '../../../stores/nav.store';
import authStore from '../../../stores/auth.store';
import tokenStore from '../../../stores/token.store';
import i18n from '../../../stores/i18n.store';

import {Utils} from '../../../utils/core/utils';
import {renderI18n} from '../../../utils/core/i18n.utils';

import {EnvironmentDeckDeckGoConfig} from '../../../types/core/environment-config';

import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';

@Component({
  tag: 'app-signin',
  styleUrl: 'app-signin.scss'
})
export class AppSignIn {
  @Element() el: HTMLElement;

  @Prop()
  redirect: string;

  @Prop()
  redirectId: string;

  @State()
  private signInInProgress: boolean = false;

  async componentDidLoad() {
    await this.setupFirebaseUI();
  }

  async disconnectedCallback() {
    const ui = firebaseui.auth.AuthUI.getInstance();
    if (ui) {
      await ui.delete();
    }
  }

  @Watch('redirect')
  async watchRedirect() {
    await this.saveRedirect();
  }

  async setupFirebaseUI() {
    this.signInInProgress = false;

    await Utils.injectJS({
      id: 'firebase-ui-script',
      src: 'https://www.gstatic.com/firebasejs/ui/4.8.0/firebase-ui-auth.js'
    });
    await Utils.injectCSS('firebase-ui-css', 'https://www.gstatic.com/firebasejs/ui/4.8.0/firebase-ui-auth.css');

    const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

    const appUrl: string = deckDeckGoConfig.appUrl;

    const signInOptions = [];

    // GitHub scope
    signInOptions.push({
      provider: firebase.auth.GithubAuthProvider.PROVIDER_ID,
      scopes: ['public_repo']
    });

    signInOptions.push(firebase.auth.GoogleAuthProvider.PROVIDER_ID);

    signInOptions.push(firebase.auth.EmailAuthProvider.PROVIDER_ID);

    const uiConfig = {
      signInFlow: 'redirect',
      signInSuccessUrl: appUrl,
      signInOptions: signInOptions,
      // tosUrl and privacyPolicyUrl accept either url string or a callback
      // function.
      // Terms of service url/callback.
      tosUrl: 'https://deckdeckgo.com/terms',
      // Privacy policy url/callback.
      privacyPolicyUrl: 'https://deckdeckgo.com/privacy',
      credentialHelper: firebaseui.auth.CredentialHelper.GOOGLE_YOLO,
      autoUpgradeAnonymousUsers: false,
      callbacks: {
        signInSuccessWithAuthResult: (authResult, _redirectUrl) => {
          this.signInInProgress = true;

          this.saveGithubCredentials(authResult);

          // TODO: We do not need the merge info anymore

          this.navigateRedirect();

          return false;
        }
      }
    };

    // @ts-ignore
    window['firebase'] = firebase;

    await this.saveRedirect();

    const ui = firebaseui.auth.AuthUI.getInstance() || new firebaseui.auth.AuthUI(firebase.auth());

    if (!ui.isPendingRedirect()) {
      ui.reset();
    }

    // The start method will wait until the DOM is loaded.
    ui.start('#firebaseui-auth-container', uiConfig);
  }

  private async saveRedirect() {
    const mergeInfo: MergeInformation = JSON.parse(localStorage.getItem('deckdeckgo_redirect_info')) as MergeInformation;

    if (mergeInfo && mergeInfo.userId && mergeInfo.userToken) {
      return;
    }

    localStorage.setItem('deckdeckgo_redirect', this.redirect ? this.redirect : '/');

    let token: string | null = null;
    if (authStore.state.authUser) {
      token = await firebase.auth().currentUser?.getIdToken();
    }

    localStorage.setItem(
      'deckdeckgo_redirect_info',
      JSON.stringify({
        deckId: this.redirectId ? this.redirectId : null,
        userId: authStore.state.authUser ? authStore.state.authUser.uid : null,
        userToken: token,
        anonymous: authStore.state.authUser ? authStore.state.authUser.anonymous : true
      })
    );
  }

  private navigateRedirect(redirectStatus: 'success' | 'failure' = 'success') {
    const redirectUrl: string = localStorage.getItem('deckdeckgo_redirect');
    const mergeInfo: MergeInformation = JSON.parse(localStorage.getItem('deckdeckgo_redirect_info')) as MergeInformation;

    localStorage.removeItem('deckdeckgo_redirect');
    localStorage.removeItem('deckdeckgo_redirect_info');

    this.navigateRoot(redirectUrl, redirectStatus, NavDirection.RELOAD, mergeInfo);
  }

  private navigateRoot(redirectUrl: string, redirectStatus: 'success' | 'failure', direction: NavDirection, mergeInfo: MergeInformation) {
    // TODO: That's ugly
    // prettier-ignore
    const url: string = !redirectUrl || redirectUrl.trim() === '' || redirectUrl.trim() === '/' ? '/' : '/' + redirectUrl + (!mergeInfo || !mergeInfo.deckId || mergeInfo.deckId.trim() === '' || mergeInfo.deckId.trim() === '/' ? '' : '/' + mergeInfo.deckId);

    // Do not push a new page but reload as we might later face a DOM with contains two firebaseui which would not work
    navStore.state.nav = {
      url: url + `?signin=${redirectStatus}`,
      direction
    };
  }

  async navigateBack() {
    navStore.state.nav = {
      direction: NavDirection.BACK
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
    return [
      <main class="ion-padding fit">
        {this.renderBackButton()}

        {this.renderMsg()}

        {this.renderGitHub()}

        <div id="firebaseui-auth-container"></div>

        <p class="ion-text-center ion-padding-start ion-padding-end">
          <small>{i18n.state.core.free_open_source}</small>
        </p>
      </main>
    ];
  }

  private renderMsg() {
    return [
      <h1 class="ion-text-center ion-padding-start ion-padding-end">{i18n.state.sign_in.hi}</h1>,
      <p class="ion-text-center ion-padding">{i18n.state.sign_in.why}</p>
    ];
  }

  private renderBackButton() {
    if (this.signInInProgress) {
      return undefined;
    } else {
      return (
        <ion-buttons class="back">
          <ion-button onClick={() => this.navigateBack()} color="dark" aria-label={i18n.state.core.close}>
            <ion-icon src="/assets/icons/ionicons/close.svg"></ion-icon>
          </ion-button>
        </ion-buttons>
      );
    }
  }

  private renderGitHub() {
    return (
      <p class="ion-text-center ion-padding-start ion-padding-end ion-padding-bottom">
        {renderI18n(i18n.state.sign_in.additionally, {placeholder: '{0}', value: <ion-icon name="logo-github"></ion-icon>})}
      </p>
    );
  }
}
