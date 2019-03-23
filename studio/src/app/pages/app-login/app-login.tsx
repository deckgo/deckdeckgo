import {Component, Element, Prop, Watch} from '@stencil/core';

import firebase from '@firebase/app';
import '@firebase/auth';

import {Utils} from '../../utils/utils';

import {EnvironmentConfigService} from '../../services/environment/environment-config.service';
import {SignInType} from '../../services/auth/auth.service';

@Component({
    tag: 'app-login',
    styleUrl: 'app-login.scss'
})
export class AppLogin {

    @Element() el: HTMLElement;

    @Prop()
    type: SignInType = SignInType.SIGNIN;

    async componentDidLoad() {
        await this.setupFirebaseUI();
    }

    @Watch('type')
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
        let redirectUrl: string = appUrl;

        if (this.type && (this.type as SignInType) === SignInType.SIGNIN_MERGE_ANONYMOUS) {
            redirectUrl += '/editor';
        }

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
        let ui = firebaseui.auth.AuthUI.getInstance() || new firebaseui.auth.AuthUI(firebase.auth());

        if (firebaseui.auth.AuthUI.getInstance()) {
            await firebaseui.auth.AuthUI.getInstance().delete();
            ui = new firebaseui.auth.AuthUI(firebase.auth());
        }

        // The start method will wait until the DOM is loaded.
        ui.reset();
        ui.start('#firebaseui-auth-container-' + this.type, uiConfig);
    }

    render() {
        return [
            <app-navigation></app-navigation>,
            <ion-content padding>
                <main padding>
                    {this.renderMsg()}

                    <div id={'firebaseui-auth-container-' + this.type}></div>

                    <p class="ion-text-center ion-padding-start ion-padding-end"><small>DeckDeckGo is free and open source 🖖</small></p>
                </main>
            </ion-content>
        ];
    }

    private renderMsg() {
        if (this.type === SignInType.SIGNIN_MERGE_ANONYMOUS) {
            return [
                <h1 class="ion-text-center ion-padding-start ion-padding-end">Oh, hi! Good to have you.</h1>,
                <p class="ion-text-center ion-padding">Sign in to extend your deck, to publish your presentation and to get soon a personalized feed of recommendations.</p>
            ]
        } else {
            return [
                <h1 class="ion-text-center ion-padding-start ion-padding-end">Oh, hi! Welcome back.</h1>,
                <p class="ion-text-center ion-padding">Sign in to publish your presentation and to get soon a personalized feed of recommendations.</p>
            ]
        }
    }
}
