import {Component, ComponentInterface, h, Prop} from '@stencil/core';

import firebase from '@firebase/app';
import '@firebase/auth';

import {Utils} from '../../utils/utils';

declare var firebaseui;

@Component({
  tag: 'deckgo-firebase-signin',
  styleUrl: 'firebase-signin.scss'
})
export class FirebaseSignIn implements ComponentInterface {
  @Prop()
  appUrl: string;

  @Prop()
  signInSuccessWithAuthResult: (authResult, _redirectUrl) => void;

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
    await Utils.injectJS({
      id: 'firebase-ui-script',
      src: 'https://www.gstatic.com/firebasejs/ui/4.8.0/firebase-ui-auth.js'
    });
    await Utils.injectCSS('firebase-ui-css', 'https://www.gstatic.com/firebasejs/ui/4.8.0/firebase-ui-auth.css');

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

  render() {
    return <div id="firebaseui-auth-container"></div>;
  }
}
