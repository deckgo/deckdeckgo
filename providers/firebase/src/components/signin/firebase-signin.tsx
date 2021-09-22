import {Component, ComponentInterface, h, Prop} from '@stencil/core';

import {injectJS, injectCSS} from '@deckdeckgo/editor';

import firebase from '@firebase/app';
import '@firebase/auth';

import type {UserCredential, OAuthCredential} from '@firebase/auth-types';

declare var firebaseui;

@Component({
  tag: 'deckgo-firebase-signin',
  styleUrl: 'firebase-signin.scss'
})
export class FirebaseSignIn implements ComponentInterface {
  @Prop()
  appUrl: string;

  @Prop()
  signInSuccess: (credentials: {uid: string | undefined; githubAccessToken: string | undefined} | undefined) => void;

  async componentDidLoad() {
    await this.setupFirebaseUI();
  }

  async disconnectedCallback() {
    const ui = firebaseui.auth.AuthUI.getInstance();
    if (ui) {
      await ui.delete();
    }
  }

  async setupFirebaseUI() {
    await injectJS({
      id: 'firebase-ui-script',
      src: 'https://www.gstatic.com/firebasejs/ui/4.8.0/firebase-ui-auth.js'
    });
    await injectCSS({
      id: 'firebase-ui-css',
      src: 'https://www.gstatic.com/firebasejs/ui/4.8.0/firebase-ui-auth.css'
    });

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
      signInSuccessUrl: this.appUrl,
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
        signInSuccessWithAuthResult: this.signInSuccessWithAuthResult
      }
    };

    // @ts-ignore
    window['firebase'] = firebase;

    const ui = firebaseui.auth.AuthUI.getInstance() || new firebaseui.auth.AuthUI(firebase.auth());

    if (!ui.isPendingRedirect()) {
      ui.reset();
    }

    // The start method will wait until the DOM is loaded.
    ui.start('#firebaseui-auth-container', uiConfig);
  }

  private signInSuccessWithAuthResult = (userCred: UserCredential, _redirectUrl): boolean => {
    const githubAccessToken: string | undefined = this.getGithubAccessToken(userCred);

    this.signInSuccess({uid: userCred?.user?.uid, githubAccessToken});

    return false;
  };

  private getGithubAccessToken(userCred: UserCredential): string | undefined {
    if (!userCred) {
      return undefined;
    }

    if (!userCred.credential || userCred.credential.providerId !== 'github.com' || !(userCred.credential as OAuthCredential).accessToken) {
      return undefined;
    }

    return (userCred.credential as OAuthCredential).accessToken;
  }

  render() {
    return <div id="firebaseui-auth-container"></div>;
  }
}
