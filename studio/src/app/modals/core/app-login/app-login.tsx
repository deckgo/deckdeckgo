import {Component, Element, Listen, Prop} from '@stencil/core';

import firebase from '@firebase/app';
import '@firebase/auth';

import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';

import {Utils} from '../../../utils/utils';
import {LoginModalType} from '../../../services/auth/auth.service';

@Component({
    tag: 'app-login',
    styleUrl: 'app-login.scss'
})
export class AppLogin {

    @Element() el: HTMLElement;

    @Prop()
    type: LoginModalType = LoginModalType.SIGNIN;

    @Prop()
    context: string;

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
        await Utils.injectJS(
            'firebase-ui-script',
            'https://cdn.firebase.com/libs/firebaseui/3.5.2/firebaseui.js'
        );
        await Utils.injectCSS(
            'firebase-ui-css',
            'https://cdn.firebase.com/libs/firebaseui/3.5.2/firebaseui.css'
        );

        const appUrl: string = EnvironmentConfigService.getInstance().get('appUrl');
        const redirectUrl: string = appUrl + (this.context ? this.context : '');

        const signInOptions = [];

        signInOptions.push(firebase.auth.GoogleAuthProvider.PROVIDER_ID);
        signInOptions.push(firebase.auth.EmailAuthProvider.PROVIDER_ID);

        const uiConfig = {
            signInFlow: 'redirect',
            signInSuccessUrl: redirectUrl,
            signInOptions: signInOptions,
            // tosUrl and privacyPolicyUrl accept either url string or a callback
            // function.
            // Terms of service url/callback.
            tosUrl: appUrl + '/terms',
            // Privacy policy url/callback.
            privacyPolicyUrl: appUrl + '/privacy',
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

                    // TODO: What to do, copy or not? merge or not merge?

                    return firebase.auth().signInAndRetrieveDataWithCredential(cred);
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
                    <ion-title text-uppercase>Welcome</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content padding>
                {this.renderMsg()}

                <div id="firebaseui-auth-container"></div>

                <p text-center padding-start padding-end><small>DeckDeckGo is free and open source 🖖</small></p>
            </ion-content>
        ];
    }

    private renderMsg() {
        if (this.type === LoginModalType.SIGNIN_MERGE_ANONYMOUS) {
            return [
                <h1 text-center padding-start padding-end>Oh, hi! Good to have you.</h1>,
                <p text-center padding>Sign in to extend your deck, to publish your presentation and to get soon a personalized feed of recommendations.</p>
            ]
        } else {
            return [
                <h1 text-center padding-start padding-end>Oh, hi! Welcome back.</h1>,
                <p text-center padding>Sign in to publish your presentation and to get soon a personalized feed of recommendations.</p>
            ]
        }
    }
}
