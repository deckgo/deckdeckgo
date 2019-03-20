import {Component, Listen, Element} from '@stencil/core';

import firebase from '@firebase/app';
import '@firebase/auth';

import {DeckdeckgoUtils} from '../../../utils/deckdeckgo-utils';

@Component({
    tag: 'app-login',
    styleUrl: 'app-login.scss'
})
export class AppLogin {

    @Element() el: HTMLElement;

    async componentDidLoad() {
        history.pushState({modal: true}, null);

        await this.setupFirebaseUI();
    }

    @Listen('window:popstate')
    async handleHardwareBackbutton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    async setupFirebaseUI() {
        await DeckdeckgoUtils.injectJS(
            'firebase-ui-script',
            'https://cdn.firebase.com/libs/firebaseui/3.5.2/firebaseui.js'
        );
        await DeckdeckgoUtils.injectCSS(
            'firebase-ui-css',
            'https://cdn.firebase.com/libs/firebaseui/3.5.2/firebaseui.css'
        );

        // TODO: clean

        var uiConfig = {
            signInFlow: 'redirect',
            signInSuccessUrl: 'http://localhost:3333',
            signInOptions: [
                // Leave the lines as is for the providers you want to offer your users.
                firebase.auth.EmailAuthProvider.PROVIDER_ID,
            ],
            // tosUrl and privacyPolicyUrl accept either url string or a callback
            // function.
            // Terms of service url/callback.
            tosUrl: 'http://localhost:3333/terms',
            // Privacy policy url/callback.
            privacyPolicyUrl: 'http://localhost:3333/privacy',
            credentialHelper: firebaseui.auth.CredentialHelper.GOOGLE_YOLO,
            autoUpgradeAnonymousUsers: true,
            callbacks: {
                // signInFailure callback must be provided to handle merge conflicts which
                // occur when an existing credential is linked to an anonymous user.
                signInFailure: (error) => {
                    // For merge conflicts, the error.code will be
                    // 'firebaseui/anonymous-upgrade-merge-conflict'.
                    if (error.code != 'firebaseui/anonymous-upgrade-merge-conflict') {
                        return Promise.resolve();
                    }
                    // The credential the user tried to sign in with.
                    var cred = error.credential;
                    // Copy data from anonymous user to permanent user and delete anonymous
                    // user.
                    // ...
                    // Finish sign-in after data is copied.
                    return firebase.auth().signInWithCredential(cred);
                }
            }
        };

        window['firebase'] = firebase;

        // Initialize the FirebaseUI Widget using Firebase.
        const ui = firebaseui.auth.AuthUI.getInstance() || new firebaseui.auth.AuthUI(firebase.auth());
        // The start method will wait until the DOM is loaded.
        ui.start('#firebaseui-auth-container', uiConfig);
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title text-uppercase>DeckDeckGo</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content padding>
                <div id="firebaseui-auth-container"></div>
            </ion-content>
        ];
    }
}
